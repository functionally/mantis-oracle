-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- Copyright   :  (c) 2021 Brian W Bush
-- License     :  MIT
--
-- Maintainer  :  Brian W Bush <code@functionally.io>
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Submitting transactions to the oracle.
--
-----------------------------------------------------------------------------


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module Mantra.Oracle.Submit (
-- * Operations
  createOracle
, deleteOracle
, writeOracle
) where


import Cardano.Api                                       (AddressAny, AddressInEra, AlonzoEra, AssetId(..), AssetName(..), AsType(AsPolicyId), BuildTxWith(..), CardanoEra(..), CardanoMode, ConsensusModeIsMultiEra(..), EraInMode(..), ExecutionUnits(..), KeyWitnessInCtx(..), Lovelace, LocalNodeConnectInfo, MultiAssetSupportedInEra(..), NetworkId, PaymentKey, PlutusScript, PlutusScriptV1, PlutusScriptVersion(..), Quantity(..), QueryInEra(..), QueryInMode(..), QueryInShelleyBasedEra(..), QueryUTxOFilter(..), ScriptData, ScriptDataJsonSchema(..), ScriptDataSupportedInEra(..), ScriptData(..), ScriptDatum(..), ScriptLanguageInEra(..), ScriptWitness(..), ScriptWitnessInCtx(..), ShelleyBasedEra(..), SigningKey, ShelleyWitnessSigningKey(..), TxAuxScripts(..), TxBody(..), TxBodyContent(..), TxCertificates(..), CollateralSupportedInEra(..), TxExtraKeyWitnesses(..), TxExtraScriptData(..), TxFee(..), TxFeesExplicitInEra(..), TxId, TxIn, TxInMode(..), TxInsCollateral(..), TxMetadata, TxMetadataInEra(..), TxMetadataJsonSchema(..), TxMetadataSupportedInEra(..), TxMintValue(..), TxOut(..), TxOutDatumHash(..), TxOutValue(..), TxScriptValidity(..), TxUpdateProposal(..), TxValidityLowerBound(..), TxValidityUpperBound(..), TxWithdrawals(..), UTxO(..), ValidityNoUpperBoundSupportedInEra(..), Value, Witness(..), anyAddressInEra, deserialiseFromRawBytes, getTxId, hashScriptData, lovelaceToQuantity, lovelaceToValue, makeTransactionBodyAutoBalance, metadataFromJson, negateValue, quantityToLovelace, queryNodeLocalState, scriptDataFromJson, selectAsset, selectLovelace, signShelleyTransaction, submitTxToNodeLocal, valueFromList, valueToList)
import Control.Monad.Except                              (throwError, liftIO)
import Data.List                                         (sortBy)
import Data.Function                                     (on)
import Data.Maybe                                        (catMaybes, fromJust)
import Data.Word                                         (Word64)
import Mantra.Oracle                                     (oracleAddressAny, plutusOracle)
import Mantra.Oracle.Types                               (Action(..), Oracle(..))
import Mantra.Types                                      (MantraM, foistMantraEither, foistMantraEitherIO, foistMantraMaybe)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult(..))
import Plutus.V1.Ledger.Value                            (AssetClass(..), CurrencySymbol(..), TokenName(..))
import PlutusTx.Builtins                                 (fromBuiltin)

import qualified Data.Aeson          as A (Value(..))
import qualified Data.HashMap.Strict as H (fromList)
import qualified Data.Map.Strict     as M (toList)
import qualified Data.Set            as S (empty, fromList, singleton)
import qualified Data.Text           as T (pack)
import qualified PlutusTx.Prelude    as P (fromEnum)


-- | Submit the transaction to create an oracle. The payment address must have *separate* eUTxOs containing the datum token and the control token; it also must have at least one other UTxO containing no tokens.
createOracle :: A.Value                          -- ^ The initial datum.
             -> Maybe Word64                     -- ^ The metadata key for the initial datum, if any.
             -> Maybe A.Value                    -- ^ The metadata message, if any.
             -> LocalNodeConnectInfo CardanoMode -- ^ The connection info for the local node.
             -> NetworkId                        -- ^ The network identifier.
             -> Oracle                           -- ^ The oracle.
             -> AddressAny                       -- ^ The payment address.
             -> SigningKey PaymentKey            -- ^ The signing key for the payment address.
             -> Lovelace                         -- ^ The maximum value for collateral.
             -> Quantity                         -- ^ The Lovelace to be sent to the script.
             -> MantraM IO TxId                  -- ^ Action to submit the creation transaction.
createOracle newData =
  operateOracle
    Nothing
    Nothing
    (Just newData)


-- | Submit the transaction to delete an oracle. The payment address must have an eUTxO containing the control token; it also must have at least one other UTxO containing no tokens.
deleteOracle :: A.Value                           -- ^ The datum currently held in the script.
             -> Maybe A.Value                     -- ^ The metadata message, if any.
             -> LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
             -> NetworkId                         -- ^ The network identifier.
             -> Oracle                            -- ^ The oracle.
             -> AddressAny                        -- ^ The payment address.
             -> SigningKey PaymentKey             -- ^ The signing key for the payment address.
             -> Lovelace                          -- ^ The maximum value for collateral.
             -> Quantity                          -- ^ The Lovelace to be sent to the script.
             -> MantraM IO TxId                   -- ^ Action to submit the deletion transaction.
deleteOracle oldData =
  operateOracle
    (Just Delete)
    (Just oldData)
    Nothing
    Nothing


-- | Submit the transaction to write new data to an oracle. The payment address must have an eUTxO containing the control token; it also must have at least one other UTxO containing no tokens.
writeOracle :: A.Value                          -- ^ The datum currently held in the script.
            -> A.Value                          -- ^ The new datum.
            -> Maybe Word64                     -- ^ The metadata key for the new datum, if any.
            -> Maybe A.Value                    -- ^ The metadata message, if any.
            -> LocalNodeConnectInfo CardanoMode -- ^ The connection info for the local node.
            -> NetworkId                        -- ^ The network identifier.
            -> Oracle                           -- ^ The oracle.
            -> AddressAny                       -- ^ The payment address.
            -> SigningKey PaymentKey            -- ^ The signing key for the payment address.
            -> Lovelace                         -- ^ The maximum value for collateral.
            -> Quantity                         -- ^ The Lovelace to be sent to the script.
            -> MantraM IO TxId                  -- ^ Action to submit the writing transaction.
writeOracle oldData newData =
  operateOracle
    (Just Write)
    (Just oldData)
    (Just newData)


-- | Submit a transaction to operate the oracle.
operateOracle :: Maybe Action                     -- ^ The redeemer, if any.
              -> Maybe A.Value                    -- ^ The datum currently held in the script, if any.
              -> Maybe A.Value                    -- ^ The new datum, if any.
              -> Maybe Word64                     -- ^ The metadata key for the new datum, if any
              -> Maybe A.Value                    -- ^ The metadata message, if any.
              -> LocalNodeConnectInfo CardanoMode -- ^ The connection info for the local node.
              -> NetworkId                        -- ^ The network identifier.
              -> Oracle                           -- ^ The oracle.
              -> AddressAny                       -- ^ The payment address.
              -> SigningKey PaymentKey            -- ^ The signing key for the payment address.
              -> Lovelace                         -- ^ The maximum value for collateral.
              -> Quantity                         -- ^ The Lovelace to be sent to the script.
              -> MantraM IO TxId                  -- ^ Action to submit the deletion transaction.
operateOracle action oldData newData metadataKey message connection network oracle controlAddress controlSigning maxCollateral scriptLovelace =
  do
    let
      script = plutusOracle oracle
      scriptAddress = oracleAddressAny network oracle
      messageMetadata = ("674", ) <$> message
      oracleMetadata = do
                         metadataKey' <- metadataKey
                         newData' <- newData
                         return (T.pack $ show metadataKey', newData')
    controlAsset <- classToId $ controlToken oracle
    datumAsset   <- classToId $ datumToken   oracle
    metadata <-
      case catMaybes [messageMetadata, oracleMetadata] of
        []        -> return Nothing
        metadata' -> foistMantraEither
                       . fmap Just
                       . metadataFromJson TxMetadataJsonNoSchema
                       . A.Object
                       $ H.fromList metadata'
    datumUTxOs <-
      findUTxO connection (maybe controlAddress (const scriptAddress) oldData)
        $ \value -> selectAsset value datumAsset == 1
    (datumTxIn, datumValue) <-
      case datumUTxOs of
        (datumTxIn, TxOut _ (TxOutValue _ datumValue) _) : _ -> return (datumTxIn, datumValue)
        _                                                    -> throwError
                                                                  $ "UTxO with single datum token not found: "
                                                                  ++ show datumUTxOs
    controlUTxOs <-
      findUTxO connection controlAddress
        $ \value -> selectAsset value controlAsset == 1
    (controlTxIn, controlValue) <-
      case controlUTxOs of
        (controlTxIn, TxOut _ (TxOutValue _ controlValue) _) : _ -> return (controlTxIn, controlValue)
        _                                                        -> throwError
                                                                      $ "UTxO with single control token not found: "
                                                                      ++ show controlUTxOs
    plainUTxOs <-
      findUTxO connection controlAddress
        $ \value -> length (valueToList value) == 1
    collateralTxIn <-
      case filter (\(_, TxOut _ (TxOutValue _ value) _) -> selectLovelace value <= maxCollateral) plainUTxOs of
        (txIn, _) : _ -> return txIn
        _             -> throwError
                           $ "No tokenless UTxO with less than maximum collateral found: "
                           ++ show plainUTxOs
    let
      total =
           datumValue
        <> controlValue
        <> mconcat [value | (_, TxOut _ (TxOutValue _ value) _) <- plainUTxOs]
      datumValue' = valueFromList [(AdaAssetId, scriptLovelace), (datumAsset, 1)]
      Quantity change =
        lovelaceToQuantity
          . selectLovelace
          $ total <> negateValue datumValue'
      changeValue =
        lovelaceToValue
          . quantityToLovelace
          . Quantity
          $ change `div` 2
      controlValue' = total <> negateValue (datumValue' <> changeValue)
    body <-
      build
        action
        connection
        script
        scriptAddress
        controlAddress
        (datumTxIn, datumValue')
        (controlTxIn, controlValue')
        plainUTxOs
        (collateralTxIn, maxCollateral)
        (oldData, newData)
        metadata
    submit connection body controlSigning


-- | Build an oracle transaction.
build :: Maybe Action                     -- ^ The redeemer, if any.
      -> LocalNodeConnectInfo CardanoMode -- ^ The connection info for the local node.
      -> PlutusScript PlutusScriptV1      -- ^ The Plutus script.
      -> AddressAny                       -- ^ The address of the Plutus script.
      -> AddressAny                       -- ^ The payment address.
      -> (TxIn, Value)                    -- ^ The UTxO with containing the datum token.
      -> (TxIn, Value)                    -- ^ The UTxO containing the control token.
      -> [(TxIn, a)]                      -- ^ The UTxOs to be consumed by the transaction.
      -> (TxIn, Lovelace)                 -- ^ The UTXO for collateral and the value for new collateral.
      -> (Maybe A.Value, Maybe A.Value)   -- ^ The existing datum and the datum to replace it, if any.
      -> Maybe TxMetadata                 -- ^ The message metadata, if any.
      -> MantraM IO (TxBody AlonzoEra)    -- ^ The action to build the transaction.
build action connection script scriptAddress controlAddress (datumTxIn,  datumValue) (controlTxIn, controlValue) plainUTxOs (collateralTxIn, collateralValue) (oldData, newData) metadata =
  do
    oldData' <- datumFromJSON oldData
    newData' <- datumFromJSON newData
    scriptAddress'  <- asAlonzoAddress "Failed to convert script address."  scriptAddress
    controlAddress' <- asAlonzoAddress "Failed to convert control address." controlAddress
    start    <- queryAny    connection   QuerySystemStart
    history  <- queryAny    connection $ QueryEraHistory CardanoModeIsMultiEra
    protocol <- queryAlonzo connection   QueryProtocolParameters
    utxo <-
      queryAlonzo connection
        . QueryUTxO
        . QueryUTxOByTxIn
        . S.fromList
        $ datumTxIn
        : controlTxIn
        : [txIn | (txIn, _) <- plainUTxOs]
    let
      txIns =
          (
            datumTxIn
          , BuildTxWith
              $ maybe
                  (KeyWitness KeyWitnessForSpending)
                  (
                    \oldData'' ->
                      ScriptWitness ScriptWitnessForSpending
                        $ PlutusScriptWitness
                          PlutusScriptV1InAlonzo
                          PlutusScriptV1
                          script
                          (ScriptDatumForTxIn oldData'')
                          (ScriptDataNumber . P.fromEnum $ fromJust action)
                          (ExecutionUnits 0 0)
                  )
                  oldData'
          )
        : (
            controlTxIn
          , BuildTxWith $ KeyWitness KeyWitnessForSpending
          )
        : [
            (
              txIn
            , BuildTxWith $ KeyWitness KeyWitnessForSpending
            )
          |
            (txIn, _) <- plainUTxOs
          ]
      txOuts =
        [
          TxOut
            (maybe controlAddress' (const scriptAddress') newData')
            (TxOutValue MultiAssetInAlonzoEra datumValue)
            (maybe TxOutDatumHashNone (TxOutDatumHash ScriptDataInAlonzoEra . hashScriptData) newData')
        , TxOut
            controlAddress'
            (TxOutValue MultiAssetInAlonzoEra controlValue)
            TxOutDatumHashNone
        , TxOut
            controlAddress'
            (TxOutValue MultiAssetInAlonzoEra $ lovelaceToValue collateralValue)
            TxOutDatumHashNone
        ]
      txInsCollateral   = TxInsCollateral CollateralInAlonzoEra [collateralTxIn]
      txFee             = TxFeeExplicit TxFeesExplicitInAlonzoEra 0
      txValidityRange   = (TxValidityNoLowerBound, TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra)
      txMetadata        = maybe TxMetadataNone (TxMetadataInEra TxMetadataInAlonzoEra) metadata
      txAuxScripts      = TxAuxScriptsNone
      txExtraScriptData = BuildTxWith TxExtraScriptDataNone
      txExtraKeyWits    = TxExtraKeyWitnessesNone
      txProtocolParams  = BuildTxWith $ Just protocol
      txWithdrawals     = TxWithdrawalsNone
      txCertificates    = TxCertificatesNone
      txUpdateProposal  = TxUpdateProposalNone
      txMintValue       = TxMintNone
      txScriptValidity  = TxScriptValidityNone
    foistMantraEither
      $ makeTransactionBodyAutoBalance
          AlonzoEraInCardanoMode
          start
          history
          protocol
          S.empty
          utxo
          TxBodyContent{..}
          controlAddress'
          Nothing


-- | Sign and submit a transaction.
submit :: LocalNodeConnectInfo CardanoMode -- ^ The connection info for the local node.
       -> TxBody AlonzoEra                 -- ^ The transaction body.
       -> SigningKey PaymentKey            -- ^ The signing key.
       -> MantraM IO TxId                  -- ^ The action to submit the transaction.
submit connection body controlSigning =
  do
    let
      tx =
        signShelleyTransaction
          body
          [
            WitnessPaymentKey controlSigning
          ]
    result <-
      liftIO
        . submitTxToNodeLocal connection
        $ TxInMode tx AlonzoEraInCardanoMode
    case result of
      SubmitSuccess     -> return $ getTxId body
      SubmitFail reason -> throwError $ show reason


-- | Read JSON data as script data.
datumFromJSON :: Maybe A.Value                 -- ^ The JSON, if any.
              -> MantraM IO (Maybe ScriptData) -- ^ Action for converting the JSON to script data.
datumFromJSON =
  foistMantraEither
    . maybe
      (return Nothing)
      (fmap Just . scriptDataFromJson ScriptDataJsonNoSchema)


-- | Convert an address to Alonzo.
asAlonzoAddress :: String                              -- ^ The error message.
                -> AddressAny                          -- ^ The address.
                -> MantraM IO (AddressInEra AlonzoEra) -- ^ Action for converting the address.
asAlonzoAddress message =
  foistMantraMaybe message
    . anyAddressInEra AlonzoEra


-- | Query the node.
queryAny :: LocalNodeConnectInfo CardanoMode -- ^ The connection info for the local node.
         -> QueryInMode CardanoMode a        -- ^ The query.
         -> MantraM IO a                     -- ^ Action for running the query.
queryAny connection =
 foistMantraEitherIO
   . queryNodeLocalState connection Nothing

queryAlonzo :: LocalNodeConnectInfo CardanoMode   -- ^ The connection info for the local node.
            -> QueryInShelleyBasedEra AlonzoEra a -- ^ The query.
            -> MantraM IO a                       -- ^ Action for running the query.
queryAlonzo connection =
  foistMantraEitherIO'
    . queryNodeLocalState connection Nothing
    . QueryInEra AlonzoEraInCardanoMode
    . QueryInShelleyBasedEra ShelleyBasedEraAlonzo


-- | Find the UTxOs meeting a criterion, and sort them by increasing value.
findUTxO :: LocalNodeConnectInfo CardanoMode     -- ^ The connection info for the local node.
         -> AddressAny                           -- ^ The address to query.
         -> (Value -> Bool)                      -- ^ The condition on values in the UTxO.
         -> MantraM IO [(TxIn, TxOut AlonzoEra)] -- ^ The action to find the UTxOs.
findUTxO connection address condition =
  do
    let
      compare' (_, TxOut _ (TxOutValue _ value) _) (_, TxOut _ (TxOutValue _ value') _) =
        (compare `on` selectLovelace) value value'
      compare' _ _ = EQ
    UTxO utxos <-
      queryAlonzo connection
        . QueryUTxO
        . QueryUTxOByAddress
        $ S.singleton address
    return
      $ sortBy compare'
      [
        utxo
      |
        utxo@(_, TxOut _ (TxOutValue _ value) _) <- M.toList utxos
      , condition value
      ]


-- | Convert a Plutus `AssetClass` to a Cardano `AssetId`.
classToId :: AssetClass         -- ^ The asset class.
          -> MantraM IO AssetId -- ^ The asset identifier.
classToId (AssetClass (CurrencySymbol policy, TokenName name)) =
   do
     policy' <-
       foistMantraMaybe "Failed to convert policy."
         . deserialiseFromRawBytes AsPolicyId
         $ fromBuiltin policy
     return
       . AssetId policy'
       . AssetName
       $ fromBuiltin name


-- | Hoist doublely nested `Either`s.
foistMantraEitherIO' :: Show e
                     => Show e'
                     => IO (Either e (Either e' a)) -- ^ The action with the nested `Either`s.
                     -> MantraM IO a                -- ^ The hoisted action.
foistMantraEitherIO' a =
  do
    a' <- foistMantraEitherIO a
    foistMantraEither a'

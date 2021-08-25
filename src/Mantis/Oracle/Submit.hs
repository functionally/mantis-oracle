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


module Mantis.Oracle.Submit (
  createOracle
, deleteOracle
, writeOracle
) where


import Cardano.Api
import Control.Monad.Except                              (throwError, liftIO)
import Data.List                                         (sortBy)
import Data.Maybe                                        (catMaybes, fromJust)
import Data.Word                                         (Word64)
import Ledger.Value                                      (AssetClass(..), CurrencySymbol(..), TokenName(..))
import Mantis.Oracle                                     (oracleAddress, plutusOracle)
import Mantis.Oracle.Types                               (Action(..), Oracle(..))
import Mantis.Types                                      (MantisM, foistMantisEither, foistMantisEitherIO, foistMantisMaybe)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult(..))
import PlutusTx.Builtins                                 (fromBuiltin)

import qualified Data.Aeson          as A
import qualified Data.HashMap.Strict as H
import qualified Data.Map.Strict     as M
import qualified Data.Set            as S
import qualified Data.Text           as T
import qualified PlutusTx.Prelude    as P


-- | Submit the transaction to create an oracle.
createOracle :: A.Value                          -- ^ The initial datum.
             -> Maybe Word64                     -- ^ The metadata key for the initial datum, if any.
             -> Maybe A.Value                    -- ^ The metadata message, if any.
             -> LocalNodeConnectInfo CardanoMode -- ^ The connection info for the local node.
             -> NetworkId                        -- ^ The network identifier.
             -> Oracle                           -- ^ The oracle.
             -> AddressAny                       -- ^ The payment address.
             -> SigningKey PaymentKey            -- ^ The signing key for the payment address.
             -> Quantity                         -- ^ The Lovelace to be sent to the script.
             -> MantisM IO TxId                  -- ^ Action to submit the creation transaction.
createOracle newData =
  operateOracle
    Nothing
    Nothing
    (Just newData)


-- | Submit the transaction to delete an oracle.
deleteOracle :: A.Value                           -- ^ The datum currently held in the script.
             -> Maybe A.Value                     -- ^ The metadata message, if any.
             -> LocalNodeConnectInfo CardanoMode  -- ^ The connection info for the local node.
             -> NetworkId                         -- ^ The network identifier.
             -> Oracle                            -- ^ The oracle.
             -> AddressAny                        -- ^ The payment address.
             -> SigningKey PaymentKey             -- ^ The signing key for the payment address.
             -> Quantity                          -- ^ The Lovelace to be sent to the script.
             -> MantisM IO TxId                   -- ^ Action to submit the deletion transaction.
deleteOracle oldData =
  operateOracle
    (Just Delete)
    (Just oldData)
    Nothing
    Nothing


-- | Submit the transaction to write new data to an oracle.
writeOracle :: A.Value                          -- ^ The datum currently held in the script. 
            -> A.Value                          -- ^ The new datum.                              
            -> Maybe Word64                     -- ^ The metadata key for the new datum, if any.
            -> Maybe A.Value                    -- ^ The metadata message, if any.              
            -> LocalNodeConnectInfo CardanoMode -- ^ The connection info for the local node.
            -> NetworkId                        -- ^ The network identifier.
            -> Oracle                           -- ^ The oracle.
            -> AddressAny                       -- ^ The payment address.
            -> SigningKey PaymentKey            -- ^ The signing key for the payment address.
            -> Quantity                         -- ^ The Lovelace to be sent to the script.
            -> MantisM IO TxId                  -- ^ Action to submit the writing transaction.
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
              -> Quantity                         -- ^ The Lovelace to be sent to the script.
              -> MantisM IO TxId                  -- ^ Action to submit the deletion transaction.
operateOracle action oldData newData metadataKey message connection network oracle controlAddress controlSigning minLovelace =
  do
    let
      script = plutusOracle oracle
      scriptAddress = oracleAddress network oracle
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
        metadata' -> foistMantisEither
                       . fmap Just
                       . metadataFromJson TxMetadataJsonNoSchema
                       . A.Object
                       $ H.fromList metadata'
    [(datumTxIn, TxOut _ (TxOutValue _ datumValue) _)] <-
      findUTxO connection (maybe controlAddress (const scriptAddress) oldData)
        $ \value -> selectAsset value datumAsset > 0
    [(controlTxIn, TxOut _ (TxOutValue _ controlValue) _)] <-
      findUTxO connection controlAddress
        $ \value -> selectAsset value controlAsset > 0
    plainUTxOs@((collateralTxIn, _) : _) <-
      findUTxO connection controlAddress
        $ \value -> length (valueToList value) == 1
    let
      total =
           datumValue
        <> controlValue
        <> mconcat [value | (_, TxOut _ (TxOutValue _ value) _) <- plainUTxOs]
      datumValue' = valueFromList [(AdaAssetId, minLovelace), (datumAsset, 1)]
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
        collateralTxIn
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
      -> TxIn                             -- ^ The UTxO for collateral.
      -> (Maybe A.Value, Maybe A.Value)   -- ^ The existing datum and the datum to replace it, if any.
      -> Maybe TxMetadata                 -- ^ The message metadata, if any.
      -> MantisM IO (TxBody AlonzoEra)    -- ^ The action to build the transaction.
build action connection script scriptAddress controlAddress (datumTxIn,  datumValue) (controlTxIn, controlValue) plainUTxOs collateralTxIn (oldData, newData) metadata =
  do
    oldData' <-
      foistMantisEither
        $ maybe
          (return Nothing)
          (fmap Just . scriptDataFromJson ScriptDataJsonNoSchema)
          oldData
    newData' <-
      foistMantisEither
        $ maybe
          (return Nothing)
          (fmap Just . scriptDataFromJson ScriptDataJsonNoSchema)
          newData
    scriptAddress' <-
      foistMantisMaybe "Failed to convert script address."
        $ anyAddressInEra AlonzoEra scriptAddress
    controlAddress' <-
      foistMantisMaybe "Failed to convert control address."
        $ anyAddressInEra AlonzoEra controlAddress
    start <-
      foistMantisEitherIO
        $ queryNodeLocalState connection Nothing QuerySystemStart
    history <-
      foistMantisEitherIO
        . queryNodeLocalState connection Nothing
        $ QueryEraHistory CardanoModeIsMultiEra
    protocol <-
      foistMantisEitherIO'
        . queryNodeLocalState connection Nothing
        . QueryInEra AlonzoEraInCardanoMode
        $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo QueryProtocolParameters
    utxo <-
      foistMantisEitherIO'
        . queryNodeLocalState connection Nothing
        . QueryInEra AlonzoEraInCardanoMode
        . QueryInShelleyBasedEra ShelleyBasedEraAlonzo  
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
      txScriptValidity  = BuildTxWith TxScriptValidityNone 
    foistMantisEither
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
       -> MantisM IO TxId                  -- ^ The action to submit the transaction.
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


-- | Find the UTxOs meeting a criterion.
findUTxO :: LocalNodeConnectInfo CardanoMode     -- ^ The connection info for the local node.
         -> AddressAny                           -- ^ The address to query.
         -> (Value -> Bool)                      -- ^ The condition on values in the UTxO.
         -> MantisM IO [(TxIn, TxOut AlonzoEra)] -- ^ The action to find the UTxOs.
findUTxO localConnInfo address condition =
  do
    let
      query =
        QueryInEra AlonzoEraInCardanoMode
          . QueryInShelleyBasedEra ShelleyBasedEraAlonzo
          . QueryUTxO
          . QueryUTxOByAddress
          $ S.singleton address
      compare' (_, TxOut _ (TxOutValue _ value) _) (_, TxOut _ (TxOutValue _ value') _) =
        selectLovelace value' `compare` selectLovelace value
      compare' _ _ = EQ
    UTxO utxos <-
      foistMantisEitherIO'
        $ queryNodeLocalState localConnInfo Nothing query
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
          -> MantisM IO AssetId -- ^ The asset identifier.
classToId (AssetClass (CurrencySymbol policy, TokenName name)) =
   do
     policy' <-
       foistMantisMaybe "Failed to convert policy."
         . deserialiseFromRawBytes AsPolicyId
         $ fromBuiltin policy
     return
       . AssetId policy'
       . AssetName
       $ fromBuiltin name


-- | Hoist doublely nested `Either`s.
foistMantisEitherIO' :: Show e
                     => Show e'
                     => IO (Either e (Either e' a)) -- ^ The action with the nested `Either`s.
                     -> MantisM IO a                -- ^ The hoisted action.
foistMantisEitherIO' a =
  do
    a' <- foistMantisEitherIO a
    foistMantisEither a'

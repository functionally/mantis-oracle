
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}


module Mantis.Oracle.Submit (
  writeOracle
) where


import Cardano.Api                                        -- (AddressAny, AlonzoEra, AssetId(..), AssetName(..), AsType(..), BuildTxWith(..), CardanoMode, CollateralSupportedInEra, EraInMode(..), LocalNodeConnectInfo, MultiAssetSupportedInEra(..), NetworkId, PaymentCredential(..), PaymentKey, PolicyId(..), PlutusScript, PlutusScriptV1, QueryInEra(..), QueryInShelleyBasedEra(..), QueryUTxOFilter(..), ScriptDataSupportedInEra(..), ShelleyBasedEra(..), ShelleyWitnessSigningKey(..), SigningKey, StakeAddressReference(..), TxAuxScripts(..), TxBody, TxBodyContent(..), TxCertificates(..), TxExtraKeyWitnesses(..), TxExtraScriptData(..), TxFee(..), TxId, TxIn, TxInMode(..), TxInsCollateral, TxMetadata, TxMetadataInEra(..), TxMintValue(..), TxOut(..), TxOutValue(..), TxScriptValidity(..), TxUpdateProposal(..), TxValidityLowerBound(..), TxValidityUpperBound(..), TxWithdrawals(..), UTxO(..), Value, ValidityNoUpperBoundSupportedInEra(..), anyAddressInEra, deserialiseFromRawBytesHex, getTxId, getVerificationKey, hashScriptData, makeShelleyAddress, makeTransactionBodyAutoBalance, metadataFromJson, negateValue, queryNodeLocalState, scriptDataFromJson, selectAsset, selectLovelace, signShelleyTransaction, submitTxToNodeLocal, toAddressAny, valueFromList, valueToList, verificationKeyHash)
import Control.Monad.Except                              (throwError, liftIO)
import Data.List                                         (sortBy)
import Data.Maybe                                        (catMaybes)
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


writeOracle :: LocalNodeConnectInfo CardanoMode
            -> NetworkId
            -> Oracle
            -> AddressAny
            -> SigningKey PaymentKey
            -> Quantity
            -> A.Value
            -> A.Value
            -> Maybe Word64
            -> Maybe A.Value
            -> MantisM IO TxId
writeOracle connection network oracle controlAddress controlSigning minLovelace oldData newData metadataKey message =
  do
    let
      script = plutusOracle oracle
      scriptAddress = oracleAddress network oracle
      messageMetadata = ("674", ) <$> message
      oracleMetadata = (, newData) . T.pack . show <$> metadataKey
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
      findUTxO connection scriptAddress
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


build :: LocalNodeConnectInfo CardanoMode
      -> PlutusScript PlutusScriptV1
      -> AddressAny
      -> AddressAny
      -> (TxIn, Value)
      -> (TxIn, Value)
      -> [(TxIn, a)]
      -> TxIn
      -> (A.Value, A.Value)
      -> Maybe TxMetadata
      -> MantisM IO (TxBody AlonzoEra)
build connection script scriptAddress controlAddress (datumTxIn,  datumValue) (controlTxIn, controlValue) plainUTxOs collateralTxIn (oldData, newData) metadata =
  do
    oldData' <-
      foistMantisEither
        $ scriptDataFromJson ScriptDataJsonNoSchema oldData
    newData' <-
      foistMantisEither
        $ scriptDataFromJson ScriptDataJsonNoSchema newData
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
              . ScriptWitness ScriptWitnessForSpending
              $ PlutusScriptWitness
                PlutusScriptV1InAlonzo
                PlutusScriptV1
                script
                (ScriptDatumForTxIn oldData')
                (ScriptDataNumber $ P.fromEnum Write)
                (ExecutionUnits 0 0)
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
            scriptAddress'
            (TxOutValue MultiAssetInAlonzoEra datumValue)
            (TxOutDatumHash ScriptDataInAlonzoEra $ hashScriptData newData')
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


submit :: LocalNodeConnectInfo CardanoMode
       -> TxBody AlonzoEra
       -> SigningKey PaymentKey
       -> MantisM IO TxId
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


findUTxO :: LocalNodeConnectInfo CardanoMode
         -> AddressAny
         -> (Value -> Bool)
         -> MantisM IO [(TxIn, TxOut AlonzoEra)]
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
        selectLovelace value `compare` selectLovelace value'
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


classToId :: AssetClass
          -> MantisM IO AssetId
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


foistMantisEitherIO' :: Show e
                     => Show e'
                     => IO (Either e (Either e' a))
                     -> MantisM IO a
foistMantisEitherIO' a =
  do
    a' <- foistMantisEitherIO a
    foistMantisEither a'

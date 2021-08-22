
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}


module Mantis.Oracle.Submit
where


import Cardano.Api
import Data.List (sortBy)
import Ledger.Value(CurrencySymbol(..), assetClass)
import Mantis.Oracle (oracleAddress, plutusOracle)
import Mantis.Oracle.Types (Parameters(..), makeOracle)
import Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult(..))
import PlutusTx.Prelude (toBuiltin)

import qualified Data.Map.Strict as M
import qualified Data.Set as S


test :: IO ()
test =
  do
    let
      network = Testnet $ NetworkMagic 8
      Just policy = deserialiseFromRawBytesHex AsPolicyId "13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1"
      policy' = CurrencySymbol . toBuiltin $ serialiseToRawBytes policy
      oracle =
        makeOracle
        $ Parameters
          {
            controlParameter = assetClass policy' "tBRIO"
          , datumParameter   = assetClass policy' "tSOFR"
          , feeToken         = assetClass policy' "tPIGY"
          , feeAmount        = 10
          }
      script = plutusOracle oracle
      scriptAddress =
        toAddressAny
          $ makeShelleyAddress
            network
            (PaymentCredentialByScript $ oracleAddress oracle)
            NoStakeAddress
      Just scriptAddress' = anyAddressInEra AlonzoEra scriptAddress
      controlAsset = AssetId policy "tBRIO"
      datumAsset   = AssetId policy "tSOFR"
      Just controlAddress = deserialiseAddress AsAddressAny "addr_test1qzwcxq9nurae3hag4xl2eazvx86ugfr7zpg0n0y7zgkj7w28gu262anw6rvqr5th53h8khjhkwrdzyq3qercygw3z6yqu36wjm"
      Just controlAddress' = anyAddressInEra AlonzoEra controlAddress
      localConnInfo =
        LocalNodeConnectInfo
        {
          localConsensusModeParams = CardanoModeParams (EpochSlots 21600)
        , localNodeNetworkId       = network
        , localNodeSocketPath      = "/data/alonzo.socket"
        }
    Right controlSigning <- readFileTextEnvelope (AsSigningKey AsPaymentKey) "/scratch/lemur.deploy.functionally.dev/alonzo/cardano-node/keys/alonzo-purple.payment-1.skey"
    Right start <-
      queryNodeLocalState localConnInfo Nothing
        QuerySystemStart
    Right history <-
      queryNodeLocalState localConnInfo Nothing
        $ QueryEraHistory CardanoModeIsMultiEra
    Right (Right protocol) <-
      queryNodeLocalState localConnInfo Nothing
        . QueryInEra AlonzoEraInCardanoMode
        $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo QueryProtocolParameters
    [(datumTxIn, TxOut _ (TxOutValue _ datumValue) _)] <-
      findUTxO localConnInfo scriptAddress
        $ \value -> selectAsset value datumAsset > 0
    putStrLn ""
    putStrLn $ "Datum UTxO: " ++ show datumTxIn
    [(controlTxIn, TxOut _ (TxOutValue _ controlValue) _)] <-
      findUTxO localConnInfo controlAddress
        $ \value -> selectAsset value controlAsset > 0
    putStrLn ""
    putStrLn $ "Control UTxO: " ++ show controlTxIn
    putStrLn ""
    plainUTxOs@((collateralTxIn, _) : _) <-
      findUTxO localConnInfo controlAddress
        $ \value -> length (valueToList value) == 1
    putStrLn ""
    putStrLn $ "Collateral UTxO: " ++ show collateralTxIn
    putStrLn ""
    Right (Right utxo) <-
      queryNodeLocalState localConnInfo Nothing
        . QueryInEra AlonzoEraInCardanoMode
        . QueryInShelleyBasedEra ShelleyBasedEraAlonzo  
        . QueryUTxO
        . QueryUTxOByTxIn
        $ S.fromList
        $ datumTxIn
        : controlTxIn
        : [txIn | (txIn, _) <- plainUTxOs]
    let
      total =
           datumValue
        <> controlValue
        <> mconcat [value | (_, TxOut _ (TxOutValue _ value) _) <- plainUTxOs]
      datumValue' = valueFromList [(AdaAssetId, 2_000_000), (datumAsset, 1)]
      feeValue = valueFromList [(AdaAssetId, 2_000_000)]
      controlValue' = total <> negateValue (datumValue' <> feeValue)
      content =
        TxBodyContent
        {
          txIns             =   (
                                  datumTxIn
                                , BuildTxWith
                                    . ScriptWitness ScriptWitnessForSpending
                                    $ PlutusScriptWitness
                                      PlutusScriptV1InAlonzo
                                      PlutusScriptV1
                                      script
                                      (ScriptDatumForTxIn $ ScriptDataBytes "Hello PIGY!")
                                      (ScriptDataNumber 2)
                                      (ExecutionUnits 0 0)
                                )
                              : (
                                  controlTxIn
                                , BuildTxWith
                                    $ KeyWitness KeyWitnessForSpending
                                )
                              : [
                                  (
                                    txIn
                                  , BuildTxWith
                                      $ KeyWitness KeyWitnessForSpending
                                  )
                                |
                                  (txIn, _) <- plainUTxOs
                                ]
        , txInsCollateral   = TxInsCollateral CollateralInAlonzoEra [collateralTxIn]
        , txOuts            = [
                                TxOut
                                  scriptAddress' 
                                  (
                                    TxOutValue MultiAssetInAlonzoEra datumValue'
                                  )
                                  (TxOutDatumHash ScriptDataInAlonzoEra . hashScriptData $ ScriptDataBytes "Hello, PIGY?")
                              , TxOut
                                  controlAddress'
                                  (
                                    TxOutValue MultiAssetInAlonzoEra controlValue'
                                  )
                                  TxOutDatumHashNone
                              ]
        , txFee             = TxFeeExplicit TxFeesExplicitInAlonzoEra 0
        , txValidityRange   = (TxValidityNoLowerBound, TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra)
        , txMetadata        = TxMetadataNone
        , txAuxScripts      = TxAuxScriptsNone
        , txExtraScriptData = BuildTxWith TxExtraScriptDataNone
        , txExtraKeyWits    = TxExtraKeyWitnessesNone
        , txProtocolParams  = BuildTxWith $ Just protocol
        , txWithdrawals     = TxWithdrawalsNone
        , txCertificates    = TxCertificatesNone
        , txUpdateProposal  = TxUpdateProposalNone
        , txMintValue       = TxMintNone
        , txScriptValidity  = BuildTxWith TxScriptValidityNone 
        }
      Right body =
        makeTransactionBodyAutoBalance
            AlonzoEraInCardanoMode
            start
            history
            protocol
            S.empty
            utxo
            content
            controlAddress'
            Nothing
      tx =
        signShelleyTransaction
          body
          [
            WitnessPaymentKey controlSigning
          ]
    result <-
      submitTxToNodeLocal localConnInfo
        $ TxInMode tx AlonzoEraInCardanoMode
    case result of
      SubmitSuccess     -> print $ getTxId body
      SubmitFail reason -> print reason


findUTxO :: LocalNodeConnectInfo CardanoMode
         -> AddressAny
         -> (Value -> Bool)
         -> IO [(TxIn, TxOut AlonzoEra)]
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
    Right (Right (UTxO utxos)) <- queryNodeLocalState localConnInfo Nothing query
    return
      $ sortBy compare'
      [
        utxo
      |
        utxo@(_, TxOut _ (TxOutValue _ value) _) <- M.toList utxos
      , condition value
      ]

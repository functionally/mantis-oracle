{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Mantis.Oracle (
  OracleScript
, oracleInstance
, oracleValidator
, oracleAddress
, findOracle
, fetchDatum
) where


import PlutusTx.Prelude hiding ((<>))

import Data.Text            (Text)
import Ledger               (Address, Datum(..), DatumHash, ScriptContext(..), TxOut(..), TxOutRef(..), TxOutTx(..), Validator, findDatum, findOwnInput, getContinuingOutputs, scriptAddress, txInInfoResolved, txData, txOutValue, valueSpent)
import Ledger.Typed.Scripts (DatumType, RedeemerType, ScriptInstance, ScriptType, validator, validatorScript, wrapValidator)
import Ledger.Value         (assetClassValueOf, geq)
import Mantis.Oracle.Types  (Action(..), Oracle(..))
import Prelude              ((<>))
import Plutus.Contract      (Contract, HasBlockchainActions, utxoAt)
import PlutusTx             (Data, applyCode, compile, fromData, liftCode, makeLift, unstableMakeIsData)

import qualified Data.Map.Strict as M (filter, lookup, toList)


makeLift ''Oracle

unstableMakeIsData ''Action


{-# INLINABLE makeValidator #-}
makeValidator :: Oracle
              -> Data
              -> Action
              -> ScriptContext
              -> Bool
makeValidator Oracle{..} inputDatum redeemer context@ScriptContext{..} =

  let

    -- Oracle input and output.
    continuingOutputs = getContinuingOutputs context
    oracleInput =
      maybe (traceError "Missing oracle input.") txInInfoResolved
        $ findOwnInput context
    oracleOutput =
      case continuingOutputs of
        [output] -> output
        _        -> traceError "Not exactly one oracle output."

    -- Datum token.
    datumTokenInput  = assetClassValueOf (txOutValue oracleInput ) datumToken == 1
    datumTokenOutput = assetClassValueOf (txOutValue oracleOutput) datumToken == 1
    singleDatum =
         traceIfFalse "Missing single oracle token input."  datumTokenInput
      && traceIfFalse "Missing single oracle token output." datumTokenOutput

    -- Datum value.
    outputDatum = fetchDatum oracleOutput (`findDatum` scriptContextTxInfo)
    datumPresent =
      traceIfFalse "Missing output datum."
        $ isJust outputDatum
    unchangedDatum =
      traceIfFalse "Datum illegally changed."
        $ outputDatum == Just inputDatum

    -- Control token.
    controlTokenInput  = assetClassValueOf (valueSpent scriptContextTxInfo        ) controlToken
    controlTokenOutput = assetClassValueOf (sum $ txOutValue <$> continuingOutputs) controlToken
    signedByControl = traceIfFalse "Not accompanied by control token."
      $ controlTokenInput > 0
    noScriptControl = traceIfFalse "Control token may not be sent to script."
      $ controlTokenOutput == 0
    controlled = signedByControl && noScriptControl

    -- Deletion.
    deleting =
         traceIfFalse "Missing single oracle token input." datumTokenInput
      && traceIfFalse "No continuing outputs allowed." (null continuingOutputs)

    -- Fee amount.
    feePaid =
      traceIfFalse "Insufficient fee."
        $ txOutValue oracleOutput `geq` (txOutValue oracleInput <> requiredFee)

  in

    case redeemer of
      Delete -> deleting && controlled
      Read   -> singleDatum  && unchangedDatum && feePaid
      Write  -> singleDatum  && controlled  && datumPresent


data OracleScript

instance ScriptType OracleScript  where
    type instance DatumType    OracleScript  = Data
    type instance RedeemerType OracleScript  = Action


oracleInstance :: Oracle
               -> ScriptInstance OracleScript
oracleInstance oracle = 
  validator @OracleScript
    ($$(compile [|| makeValidator ||]) `applyCode` liftCode oracle)
      $$(compile [|| wrap ||])
    where
      wrap = wrapValidator @Data @Action


oracleValidator :: Oracle
                -> Validator
oracleValidator = validatorScript . oracleInstance


oracleAddress :: Oracle
              -> Address
oracleAddress = scriptAddress . oracleValidator


findOracle :: HasBlockchainActions s
           => Oracle
           -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Data))
findOracle oracle@Oracle{..} =
  do
    utxos <-
      M.filter 
        (\o -> assetClassValueOf (txOutValue $ txOutTxOut o) datumToken == 1)
        <$> utxoAt (oracleAddress oracle)
    return
      $ case M.toList utxos of
          [(oref, o)] -> do
                           datum <-
                             fetchDatum (txOutTxOut o)
                                $ \dh -> M.lookup dh
                                $ txData
                                $ txOutTxTx o
                           return (oref, o, datum)
          _            -> Nothing


{-# INLINABLE fetchDatum #-}
fetchDatum :: TxOut
           -> (DatumHash -> Maybe Datum)
           -> Maybe Data
fetchDatum TxOut{..} fetch =
  do
    hash <- txOutDatumHash
    Datum datum <- fetch hash
    fromData datum

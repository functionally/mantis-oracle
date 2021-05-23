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
import Ledger               (Address, Datum(..), DatumHash, ScriptContext(..), TxOut(..), TxOutRef(..), TxOutTx(..), Validator, findDatum, findOwnInput, getContinuingOutputs, scriptAddress, txInInfoResolved, txData, txOutValue, txSignedBy)
import Ledger.Ada           (lovelaceValueOf)
import Ledger.Typed.Scripts (DatumType, RedeemerType, ScriptInstance, ScriptType, validator, validatorScript, wrapValidator)
import Ledger.Value         (assetClassValueOf, geq)
import Mantis.Oracle.Types  (Action(..), Oracle(..))
import Prelude              ((<>))
import Plutus.Contract      (Contract, HasBlockchainActions, utxoAt)
import PlutusTx             (applyCode, compile, fromData, liftCode, makeLift, unstableMakeIsData)

import qualified Data.Map.Strict as M (filter, lookup, toList)


makeLift ''Oracle

unstableMakeIsData ''Action


{-# INLINABLE makeValidator #-}
makeValidator :: Oracle
              -> Integer
              -> Action
              -> ScriptContext
              -> Bool
makeValidator Oracle{..} inputDatum redeemer context@ScriptContext{..} =
  let
    oracleInput =
      maybe (traceError "Missing oracle input.") txInInfoResolved
        $ findOwnInput context
    oracleOutput =
      case getContinuingOutputs context of
        [output] -> output
        _        -> traceError "Not exactly one oracle output."
    tokenInput  = assetClassValueOf (txOutValue oracleInput ) token == 1
    tokenOutput = assetClassValueOf (txOutValue oracleOutput) token == 1
    tokenConstraint =
         traceIfFalse "Missing single oracle token input."  tokenInput
      && traceIfFalse "Missing single oracle token output." tokenOutput
    deleteConstraint =
         traceIfFalse "Missing single oracle token input."  tokenInput
      && traceIfFalse "No continuing autputs allowed." (null $ getContinuingOutputs context)
    signedByOwner =
      traceIfFalse "Not signed by owner."
        $ scriptContextTxInfo `txSignedBy` owner
    outputDatum = fetchDatum oracleOutput (`findDatum` scriptContextTxInfo)
    datumPresent =
      traceIfFalse "Missing output datum."
        $ isJust outputDatum
    unchangedDatum =
      traceIfFalse "Datum illegally changed."
        $ outputDatum == Just inputDatum
    feePaid =
      traceIfFalse "Insufficient fee."
        $ txOutValue oracleOutput `geq` (txOutValue oracleInput <> lovelaceValueOf fee)
  in
    case redeemer of
      Delete -> deleteConstraint && signedByOwner
      Read   -> tokenConstraint  && unchangedDatum && feePaid
      Write  -> tokenConstraint  && signedByOwner  && datumPresent


data OracleScript

instance ScriptType OracleScript where
    type instance DatumType    OracleScript = Integer
    type instance RedeemerType OracleScript = Action


oracleInstance :: Oracle
               -> ScriptInstance OracleScript
oracleInstance oracle = 
  validator @OracleScript
    ($$(compile [|| makeValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode oracle) $$(PlutusTx.compile [|| wrap ||])
    where
      wrap = wrapValidator @Integer @Action


oracleValidator :: Oracle
                -> Validator
oracleValidator = validatorScript . oracleInstance


oracleAddress :: Oracle
              -> Address
oracleAddress = scriptAddress . oracleValidator


findOracle :: HasBlockchainActions s
           => Oracle
           -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Integer))
findOracle oracle@Oracle{..} =
  do
    utxos <-
      M.filter 
        (\o -> assetClassValueOf (txOutValue $ txOutTxOut o) token == 1)
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
           -> Maybe Integer
fetchDatum TxOut{..} fetch =
  do
    hash <- txOutDatumHash
    Datum datum <- fetch hash
    fromData datum

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
-- | Oracle for general data.
--
-----------------------------------------------------------------------------


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
-- * Oracle
  OracleScript
, oracleInstance
, oracleValidator
, oracleAddress
, exportOracle
-- * Access
, findOracle
, fetchDatum
) where


import PlutusTx.Prelude hiding ((<>))

import Codec.Serialise      (serialise)
import Data.Text            (Text)
import Ledger               (Address, Datum(..), DatumHash, ScriptContext(..), TxOut(..), TxOutRef(..), TxOutTx(..), Validator, findDatum, findOwnInput, getContinuingOutputs, scriptAddress, txInInfoResolved, txData, txOutValue, valueSpent)
import Ledger.Typed.Scripts (DatumType, RedeemerType, ScriptInstance, ScriptType, validator, validatorScript, wrapValidator)
import Ledger.Value         (assetClassValueOf, geq)
import Mantis.Oracle.Types  (Action(..), Oracle(..))
import Prelude              (FilePath, IO, (<>))
import Plutus.Contract      (Contract, HasBlockchainActions, utxoAt)
import PlutusTx             (Data, applyCode, compile, fromData, liftCode, makeLift, unstableMakeIsData)

import qualified Data.ByteString.Lazy as LBS (writeFile)
import qualified Data.Map.Strict      as M (filter, lookup, toList)


makeLift ''Oracle

unstableMakeIsData ''Action


{-# INLINABLE makeValidator #-}

-- | Make the validator for the oracle.
makeValidator :: Oracle         -- ^ The oracle.
              -> Data           -- ^ The datum.
              -> Action         -- ^ The redeemer.
              -> ScriptContext  -- ^ The context.
              -> Bool           -- ^ Whether the transaction is valid.
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


-- | Type for the script.
data OracleScript

instance ScriptType OracleScript  where
    type instance DatumType    OracleScript  = Data
    type instance RedeemerType OracleScript  = Action


-- | Compute the instance for an oracle.
oracleInstance :: Oracle                      -- ^ The oracle.
               -> ScriptInstance OracleScript -- ^ The instance.
oracleInstance oracle = 
  validator @OracleScript
    ($$(compile [|| makeValidator ||]) `applyCode` liftCode oracle)
      $$(compile [|| wrap ||])
    where
      wrap = wrapValidator @Data @Action


-- | Compute the validator for an oracle.
oracleValidator :: Oracle    -- ^ The oracle.
                -> Validator -- ^ The validator.
oracleValidator = validatorScript . oracleInstance


-- | Compute the address for an oracle.
oracleAddress :: Oracle  -- ^ The oracle.
              -> Address -- ^ The script address.
oracleAddress = scriptAddress . oracleValidator


-- | Find an oracle on the blockchain.
findOracle :: HasBlockchainActions s
           => Oracle                                              -- ^ The oracle.
           -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Data)) -- ^ Action for finding the oracle's UTxO and datum.
findOracle oracle@Oracle{..} =
  do
    utxos <-
      M.filter 
        (\o -> assetClassValueOf (txOutValue $ txOutTxOut o) datumToken == 1)
        <$> utxoAt (oracleAddress oracle)
    return
      $ case M.toList utxos of
          [(oref, o@TxOutTx{..})] -> do
                                       datum <-
                                         fetchDatum txOutTxOut
                                            . flip M.lookup
                                            $ txData txOutTxTx
                                       return (oref, o, datum)
          _                       -> Nothing


{-# INLINABLE fetchDatum #-}

-- | Retrieve the oracle's datum from a transaction output.
fetchDatum :: TxOut                      -- ^ The transaction output.
           -> (DatumHash -> Maybe Datum) -- ^ Function for looking up the datum, given its hash.
           -> Maybe Data                 -- ^ The datum, if it was found.
fetchDatum TxOut{..} fetch =
  do
    hash <- txOutDatumHash
    Datum datum <- fetch hash
    fromData datum


-- | Export the validator for an oracle and compute its address.
exportOracle :: FilePath   -- ^ The filename for writing the validator bytes.
             -> Oracle     -- ^ The oracle.
             -> IO Address -- ^ Action writing the validator and returning its address.
exportOracle filename oracle =
  do
    let
      a = oracleAddress oracle
      v = oracleValidator oracle
    LBS.writeFile filename
      $ serialise v
    return a

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
--, findOracle
, fetchDatum

, printRedeemers
) where


import PlutusTx.Prelude hiding ((<>))

import Codec.Serialise      (serialise)
import Data.Text            (Text)
import Ledger     --          (Address, Datum(..), DatumHash, ScriptContext(..), TxOut(..), TxOutRef(..), TxOutTx(..), Validator, findDatum, findOwnInput, getContinuingOutputs, scriptAddress, txInInfoResolved, txData, txOutValue, valueSpent)
import Ledger.Typed.Scripts -- (DatumType, RedeemerType, ScriptInstance, ScriptType, validator, validatorScript, wrapValidator)
import Ledger.Value         (assetClassValueOf, geq)
import Mantis.Oracle.Types  (Action(..), Oracle(..))
import Prelude              (FilePath, IO, (<>))
--import Plutus.Contract      (Contract, HasBlockchainActions, utxoAt)
import PlutusTx --            (Data, applyCode, compile, fromData, liftCode, makeLift, unstableMakeIsData)
import System.IO (writeFile)
import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS (toStrict, writeFile)
import qualified Data.Map.Strict      as M (filter, lookup, toList)
import Prelude (show)
import Cardano.Api.Shelley (writeFileTextEnvelope)
import System.IO (print)

import qualified Data.ByteString.Base16 as Base16 

makeLift ''Oracle

unstableMakeIsData ''Action


printRedeemers :: IO ()
printRedeemers =
  do
    print $ Base16.encode $ LBS.toStrict $ serialise $ builtinDataToData $ toBuiltinData Write


{-# INLINABLE makeValidator #-}

-- | Make the validator for the oracle.
makeValidator :: Oracle         -- ^ The oracle.
              -> BuiltinData           -- ^ The datum.
              -> Integer        -- ^ The redeemer.
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

    if redeemer == 0
    then deleting && controlled
    else if redeemer == 1 
    then singleDatum && unchangedDatum && feePaid
    else if redeemer == 2
    then singleDatum && controlled && datumPresent
    else if redeemer == -1
    then datumTokenInput
    else if redeemer == -2
    then datumTokenOutput
    else if redeemer == -3
    then singleDatum
    else if redeemer == -4
    then datumPresent
    else if redeemer == -5
    then unchangedDatum
    else if redeemer == -6
    then signedByControl
    else if redeemer == -7
    then noScriptControl
    else if redeemer == -8
    then controlled
    else if redeemer == -9
    then deleting
    else if redeemer == -10
    then feePaid
    else False



-- | Type for the script.
data OracleScript

instance ValidatorTypes OracleScript  where
    type instance DatumType    OracleScript  = BuiltinData
    type instance RedeemerType OracleScript  = Integer


-- | Compute the instance for an oracle.
oracleInstance :: Oracle                      -- ^ The oracle.
               -> TypedValidator OracleScript -- ^ The instance.
oracleInstance oracle = 
  mkTypedValidator @OracleScript
    ($$(compile [|| makeValidator ||]) `applyCode` liftCode oracle)
      $$(compile [|| wrap ||])
    where
      wrap = wrapValidator @BuiltinData @Integer


-- | Compute the validator for an oracle.
oracleValidator :: Oracle    -- ^ The oracle.
                -> Validator -- ^ The validator.
oracleValidator = validatorScript . oracleInstance


-- | Compute the address for an oracle.
oracleAddress :: Oracle  -- ^ The oracle.
              -> Address -- ^ The script address.
oracleAddress = scriptAddress . oracleValidator


-- -- | Find an oracle on the blockchain.
-- findOracle :: HasBlockchainActions s
--            => Oracle                                              -- ^ The oracle.
--            -> Contract w s Text (Maybe (TxOutRef, TxOutTx, Data)) -- ^ Action for finding the oracle's UTxO and datum.
-- findOracle oracle@Oracle{..} =
--   do
--     utxos <-
--       M.filter 
--         (\o -> assetClassValueOf (txOutValue $ txOutTxOut o) datumToken == 1)
--         <$> utxoAt (oracleAddress oracle)
--     return
--       $ case M.toList utxos of
--           [(oref, o@TxOutTx{..})] -> do
--                                        datum <-
--                                          fetchDatum txOutTxOut
--                                             . flip M.lookup
--                                             $ txData txOutTxTx
--                                        return (oref, o, datum)
--           _                       -> Nothing


{-# INLINABLE fetchDatum #-}

-- | Retrieve the oracle's datum from a transaction output.
fetchDatum :: TxOut                      -- ^ The transaction output.
           -> (DatumHash -> Maybe Datum) -- ^ Function for looking up the datum, given its hash.
           -> Maybe BuiltinData                 -- ^ The datum, if it was found.
fetchDatum TxOut{..} fetch =
  do
    hash <- txOutDatumHash
    Datum datum <- fetch hash
    fromBuiltinData datum


serialiseOracle :: Oracle
                -> SBS.ShortByteString
serialiseOracle = SBS.toShort . LBS.toStrict . serialise . unValidatorScript . oracleValidator


-- | Export the validator for an oracle and compute its address.
exportOracle :: FilePath   -- ^ The filename for writing the validator bytes.
             -> Oracle     -- ^ The oracle.
             -> IO Address -- ^ Action writing the validator and returning its address.
exportOracle filename oracle =
  do
    let
      a = oracleAddress oracle
      s = serialiseOracle oracle
    writeFileTextEnvelope filename Nothing ( PlutusScriptSerialised s :: PlutusScript PlutusScriptV1)
    return a

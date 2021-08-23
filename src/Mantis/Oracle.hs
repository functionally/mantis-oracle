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
{-# LANGUAGE CPP                   #-}
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
, plutusOracle
, exportOracle
-- * Access
#if USE_PAB
, findOracle
#endif
, fetchDatum
) where


import PlutusTx.Prelude hiding ((<>))

import Cardano.Api          (AddressAny, NetworkId, PaymentCredential(..), StakeAddressReference(..), makeShelleyAddress, toAddressAny)
import Cardano.Api.Shelley  (PlutusScript(..), PlutusScriptVersion(..), PlutusScriptV1, Script(..), hashScript, writeFileTextEnvelope)
import Codec.Serialise      (serialise)
import Control.Monad        (void)
import Ledger               (Datum(..), DatumHash, ScriptContext(..), TxOut(..), findOwnInput, getContinuingOutputs, txInInfoResolved, txOutValue, unValidatorScript, valueSpent)
import Ledger.Typed.Scripts (DatumType, RedeemerType, TypedValidator, Validator, ValidatorTypes, mkTypedValidator, validatorScript, wrapValidator)
import Ledger.Value         (assetClassValueOf, geq)
import Mantis.Oracle.Types  (Action(..), Oracle(..))
import Prelude              (FilePath, IO, (<>))
import PlutusTx             (FromData(..), ToData(..), UnsafeFromData(..), applyCode, compile, liftCode, makeLift)

import qualified Data.ByteString.Short as SBS (ShortByteString, toShort)
import qualified Data.ByteString.Lazy  as LBS (toStrict)

#if USE_PAB

import Data.Text       (Text)
import Ledger          (TxOutRef(..), TxOutTx(..), Validator, findDatum, txData)
import Plutus.Contract (Contract, HasBlockchainActions, utxoAt)
import PlutusTx        (Data, fromData)

import qualified Data.Map.Strict as M (filter, lookup, toList)

#endif


makeLift ''Oracle


-- FIXME: Temporarily map actions to integers, in order to accommodate Alonzo Purple.
#if USE_PAB
#else
instance FromData Action where
  fromBuiltinData = fmap toEnum . fromBuiltinData
instance UnsafeFromData Action where
  unsafeFromBuiltinData = toEnum . unsafeFromBuiltinData
instance ToData Action where
  toBuiltinData = toBuiltinData . fromEnum
#endif


{-# INLINABLE makeValidator #-}

-- | Make the validator for the oracle.
makeValidator :: Oracle         -- ^ The oracle.
              -> BuiltinData    -- ^ The datum.
              -> Action         -- ^ The redeemer.
              -> ScriptContext  -- ^ The context.
              -> Bool           -- ^ Whether the transaction is valid.
makeValidator Oracle{..} _ redeemer context@ScriptContext{..} =

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
    inputDatumHash = txOutDatumHash oracleInput
    outputDatumHash = txOutDatumHash oracleOutput
    datumPresent =
      traceIfFalse "Missing output datum."
        $ isJust outputDatumHash
    unchangedDatum =
      traceIfFalse "Datum illegally changed."
        $ outputDatumHash == inputDatumHash

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
      Delete  -> deleting && controlled
      Read    -> singleDatum  && unchangedDatum && feePaid
      Write   -> singleDatum  && controlled  && datumPresent
      Debug i -> if i == -1
                   then datumTokenInput
                   else if i == -2
                     then datumTokenOutput
                     else if i == -3
                       then singleDatum
                       else if i == -4
                         then isJust inputDatumHash
                         else if i == -5
                           then datumPresent
                           else if i == -6
                             then unchangedDatum
                             else if i == -7
                               then signedByControl
                               else if i == -8
                                 then noScriptControl
                                 else if i == -9
                                   then controlled
                                   else if i == -10
                                     then deleting
                                     else if i == -11
                                       then feePaid
                                       else False


-- | Type for the script.
data OracleScript

instance ValidatorTypes OracleScript  where
    type instance DatumType    OracleScript  = BuiltinData
    type instance RedeemerType OracleScript  = Action


-- | Compute the instance for an oracle.
oracleInstance :: Oracle                      -- ^ The oracle.
               -> TypedValidator OracleScript -- ^ The instance.
oracleInstance oracle =
  mkTypedValidator @OracleScript
    ($$(compile [|| makeValidator ||]) `applyCode` liftCode oracle)
      $$(compile [|| wrap ||])
    where
      wrap = wrapValidator @BuiltinData @Action


-- | Compute the validator for an oracle.
oracleValidator :: Oracle    -- ^ The oracle.
                -> Validator -- ^ The validator.
oracleValidator = validatorScript . oracleInstance


-- | Compute the address for an oracle.
oracleAddress :: NetworkId  -- ^ The network identifier.
              -> Oracle     -- ^ The oracle.
              -> AddressAny -- ^ The script address.
oracleAddress network oracle = 
  toAddressAny
    $ makeShelleyAddress network
      (
        PaymentCredentialByScript 
          . hashScript
          . PlutusScript PlutusScriptV1
          $ plutusOracle oracle
      )
      NoStakeAddress


#if USE_PAB

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

#endif


{-# INLINABLE fetchDatum #-}

-- | Retrieve the oracle's datum from a transaction output.
fetchDatum :: TxOut                      -- ^ The transaction output.
           -> (DatumHash -> Maybe Datum) -- ^ Function for looking up the datum, given its hash.
           -> Maybe BuiltinData          -- ^ The datum, if it was found.
fetchDatum TxOut{..} fetch =
  do
    hash <- txOutDatumHash
    Datum datum <- fetch hash
    fromBuiltinData datum


-- | Serialize the oracle as bytes.
serialiseOracle :: Oracle              -- ^ The oracle.
                -> SBS.ShortByteString -- ^ Its serialization.
serialiseOracle = SBS.toShort . LBS.toStrict . serialise . unValidatorScript . oracleValidator


-- | Serialise the oracle as a Plutus script.
plutusOracle :: Oracle                      -- ^ The oracle.
             -> PlutusScript PlutusScriptV1 -- ^ The Plutus script.
plutusOracle = PlutusScriptSerialised . serialiseOracle


-- | Export the validator for an oracle and compute its address.
exportOracle :: FilePath      -- ^ The filename for writing the validator bytes.
             -> NetworkId     -- ^ The network identifier.
             -> Oracle        -- ^ The oracle.
             -> IO AddressAny -- ^ Action writing the validator and returning its address.
exportOracle filename network oracle =
  do
    void
      . writeFileTextEnvelope filename Nothing
      $ plutusOracle oracle
    return
      $ oracleAddress network oracle

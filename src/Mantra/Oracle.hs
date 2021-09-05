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


{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Mantra.Oracle (
-- * Oracle
  OracleScript
, oracleInstance
, oracleValidator
, oracleAddress
, oracleAddressAny
, plutusOracle
, exportOracle
-- * Access
#if USE_PAB
, findOracle
#endif
, fetchDatum
) where


import PlutusTx.Prelude hiding ((<>))

import Cardano.Api               (AddressAny, NetworkId, PaymentCredential(..), StakeAddressReference(..), makeShelleyAddress, toAddressAny)
import Cardano.Api.Shelley       (PlutusScript(..), PlutusScriptVersion(..), PlutusScriptV1, Script(..), hashScript, writeFileTextEnvelope)
import Codec.Serialise           (serialise)
import Control.Monad             (void)
import Ledger                    (scriptAddress)
import Ledger.Typed.Scripts      (DatumType, RedeemerType, TypedValidator, Validator, ValidatorTypes, mkTypedValidator, validatorScript, wrapValidator)
import Ledger.Value              (assetClassValueOf, geq)
import Mantra.Oracle.Types       (Action(..), Oracle(..))
import Prelude                   (FilePath, IO, (<>))
import Plutus.V1.Ledger.Address  (Address, )
import Plutus.V1.Ledger.Contexts (ScriptContext(..), TxInInfo(..), TxOut(..), findOwnInput, getContinuingOutputs, valueSpent)
import Plutus.V1.Ledger.Scripts  (Datum(..), DatumHash, unValidatorScript)
import PlutusTx                  (FromData(..), ToData(..), UnsafeFromData(..), applyCode, compile, liftCode, makeLift)

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
      case findOwnInput context of
        Just input -> txInInfoResolved input
        _          -> error ()
    oracleOutput =
      case continuingOutputs of
        [output] -> output
        _        -> error ()

    -- Values.
    valueBefore = txOutValue oracleInput
    valueAfter  = txOutValue oracleOutput
    valueInput  = valueSpent scriptContextTxInfo

    -- Datum token.
    datumFromOracle = assetClassValueOf valueBefore datumToken == 1
    datumToOracle   = assetClassValueOf valueAfter  datumToken == 1

    -- Datum value.
    inputDatumHash  = txOutDatumHash oracleInput
    outputDatumHash = txOutDatumHash oracleOutput
    unchangedDatum = outputDatumHash == inputDatumHash

    -- Control token.
    controlTokenInput = assetClassValueOf valueInput controlToken
    controlToOracle   = assetClassValueOf valueAfter controlToken
    authorizedByControl = controlTokenInput > 0
    noControlToOracle   = deleting || controlToOracle == 0

    -- Deletion.
    deleting = null continuingOutputs

    -- Fee amount.
    feePaid = valueAfter `geq` (valueBefore <> requiredFee)

  in

    datumFromOracle
      && noControlToOracle
      && case redeemer of
           Delete -> deleting      && authorizedByControl
           Write  -> datumToOracle && authorizedByControl
           Read   -> datumToOracle && unchangedDatum && feePaid


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
oracleAddress :: Oracle  -- ^ The oracle.
              -> Address -- ^ The script address.
oracleAddress = scriptAddress . oracleValidator


-- | Compute the address for an oracle.
oracleAddressAny :: NetworkId  -- ^ The network identifier.
                 -> Oracle     -- ^ The oracle.
                 -> AddressAny -- ^ The script address.
oracleAddressAny network oracle =
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
      $ oracleAddressAny network oracle

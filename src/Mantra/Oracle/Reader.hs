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
-- | Example validator to read the oracle.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Mantra.Oracle.Reader (
-- * Oracle
  findOracleValue
-- * Example
, ReaderScript
, readerInstance
, readerValidator
, readerAddress
, plutusReader
, exportReader
) where


import PlutusTx.Prelude hiding ((<>))

import Cardano.Api               (AddressAny, NetworkId, PaymentCredential(..), StakeAddressReference(..), makeShelleyAddress, toAddressAny)
import Cardano.Api.Shelley       (PlutusScript(..), PlutusScriptVersion(..), PlutusScriptV1, Script(..), hashScript, writeFileTextEnvelope)
import Codec.Serialise           (serialise)
import Control.Monad             (void)
import Ledger.Typed.Scripts      (DatumType, RedeemerType, TypedValidator, ValidatorTypes, mkTypedValidator, validatorScript, wrapValidator)
import Prelude                   (FilePath, IO)
import Plutus.V1.Ledger.Contexts (ScriptContext(..), TxInInfo(..), TxInfo(..), TxOut(..), findDatum)
import Plutus.V1.Ledger.Scripts  (Datum(..), Validator, unValidatorScript)
import Plutus.V1.Ledger.Value    (AssetClass, assetClassValueOf)
import PlutusTx                  (FromData(..), applyCode, compile, liftCode)

import qualified Data.ByteString.Short as SBS (ShortByteString, toShort)
import qualified Data.ByteString.Lazy  as LBS (toStrict)


{-# INLINABLE findOracleValue #-}

-- | Find the oracle value for a transaction.
findOracleValue :: FromData a
                => AssetClass -- ^ The asset class for the datum token.
                -> TxInfo     -- ^ The transaction information.
                -> Maybe a    -- ^ The oracle value, if any.
findOracleValue token txInfo@TxInfo{..} =
  do
    let
      candidates =
        [
          candidate
        | input <- txInfoInputs
        , let candidate = txInInfoResolved input
        , assetClassValueOf (txOutValue candidate) token == 1
        ]
    TxOut{..} <-
      case candidates of
        [candidate] -> Just candidate
        _           -> Nothing
    hash <- txOutDatumHash
    Datum datum <- findDatum hash txInfo
    fromBuiltinData datum


{-# INLINABLE makeValidator #-}

-- | Make the validator for the reader.
makeValidator :: AssetClass     -- ^ The asset class for the datum token.
              -> BuiltinData    -- ^ The datum.
              -> BuiltinData    -- ^ The redeemer.
              -> ScriptContext  -- ^ The context.
              -> Bool           -- ^ Whether the transaction is valid.
makeValidator datumToken _ redeemer ScriptContext{..} =
  let
    datum = findOracleValue datumToken scriptContextTxInfo
  in
    datum == Just redeemer


-- | Type for the script.
data ReaderScript

instance ValidatorTypes ReaderScript  where
    type instance DatumType    ReaderScript  = BuiltinData
    type instance RedeemerType ReaderScript  = BuiltinData


-- | Compute the instance for an oracle.
readerInstance :: AssetClass                  -- ^ The asset class for the datum token.
               -> TypedValidator ReaderScript -- ^ The instance.
readerInstance oracle =
  mkTypedValidator @ReaderScript
    ($$(compile [|| makeValidator ||]) `applyCode` liftCode oracle)
      $$(compile [|| wrap ||])
    where
      wrap = wrapValidator @BuiltinData @BuiltinData


-- | Compute the validator for an oracle.
readerValidator :: AssetClass -- ^ The asset class for the datum token.
                -> Validator  -- ^ The validator.
readerValidator = validatorScript . readerInstance


-- | Compute the address for an oracle.
readerAddress :: NetworkId  -- ^ The network identifier.
              -> AssetClass -- ^ The asset class for the datum token.
              -> AddressAny -- ^ The script address.
readerAddress network datumToken =
  toAddressAny
    $ makeShelleyAddress network
      (
        PaymentCredentialByScript
          . hashScript
          . PlutusScript PlutusScriptV1
          $ plutusReader datumToken
      )
      NoStakeAddress


-- | Serialize the oracle as bytes.
serialiseReader :: AssetClass          -- ^ The asset class for the datum token.
                -> SBS.ShortByteString -- ^ Its serialization.
serialiseReader = SBS.toShort . LBS.toStrict . serialise . unValidatorScript . readerValidator


-- | Serialise the oracle as a Plutus script.
plutusReader :: AssetClass                  -- ^ The asset class for the datum token.
             -> PlutusScript PlutusScriptV1 -- ^ The Plutus script.
plutusReader = PlutusScriptSerialised . serialiseReader


-- | Export the validator for an oracle and compute its address.
exportReader :: FilePath      -- ^ The filename for writing the validator bytes.
             -> NetworkId     -- ^ The network identifier.
             -> AssetClass    -- ^ The asset class for the datum token.
             -> IO AddressAny -- ^ Action writing the validator and returning its address.
exportReader filename network datumToken =
  do
    void
      . writeFileTextEnvelope filename Nothing
      $ plutusReader datumToken
    return
      $ readerAddress network datumToken

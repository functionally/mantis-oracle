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
-- | Client access for the general-purpose oracle.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}


module Mantra.Oracle.Client (
-- * Script and Schema
  ClientScript
, ClientSchema
-- * Enpoints
, readOracleConstraints
, readOracle
-- * Utilities
, findOracle
, runOracleClient
) where


import PlutusTx.Prelude hiding ((<>))

import Data.Monoid          (Last (..))
import Data.Text            (Text)
import Data.Void            (Void)
import Ledger               (Datum(..), Redeemer(..), TxOutRef(..), txId, validatorHash)
import Ledger.Constraints   (ScriptLookups, TxConstraints, mustPayToOtherScript, mustSpendScriptOutput, otherScript, unspentOutputs)
import Ledger.Tx            (ChainIndexTxOut(..))
import Ledger.Typed.Scripts (DatumType, RedeemerType, ValidatorTypes)
import Ledger.Value         (assetClassValueOf)
import Mantra.Oracle        (oracleAddress, oracleValidator)
import Mantra.Oracle.Types  (Oracle(..), Action(Read))
import Plutus.Contract      (Contract, Endpoint, Promise, awaitTxConfirmed, datumFromHash, handleEndpoint, logError, logInfo, runError, submitTxConstraintsWith, tell, utxosAt)
import PlutusTx             (Data, ToData(..), builtinDataToData, dataToBuiltinData)
import Prelude              (String, (<>), show)

import qualified Data.Map.Strict as M (filter, singleton, toList)


-- | Oracle script for clients.
data ClientScript

instance ValidatorTypes ClientScript where
  type instance DatumType    ClientScript = ()
  type instance RedeemerType ClientScript = ()


-- | Compute the lookup and constraints for reading the oracle on-chain.
readOracleConstraints :: Oracle                                                               -- ^ The oracle.
                      -> Contract w s Text (Maybe (ScriptLookups a, TxConstraints i o, Data)) -- ^ Action for computing the oracle's lookups, constraints, and data.
readOracleConstraints oracle@Oracle{..} =
  let
    found (outputRef, output, datum) =
      let
         lookups = otherScript (oracleValidator oracle)
                <> unspentOutputs (M.singleton outputRef output)
         tx = mustSpendScriptOutput
                outputRef (Redeemer $ toBuiltinData Read)
           <> mustPayToOtherScript
                (validatorHash $ oracleValidator oracle)
                (Datum $ dataToBuiltinData datum)
                (_ciTxOutValue output <> requiredFee)
      in
        (lookups, tx, datum)
  in
    do
      inst <- findOracle oracle
      return $ found <$> inst


-- | Endpoint for reading the datum from the oracle.
readOracle :: Oracle                         -- ^ The oracle.
           -> Contract w s Text (Maybe Data) -- ^ Action for reading the datum.
readOracle oracle =
  let
    notFound =
      do
        logError @String "Oracle not found."
        return Nothing
    found (lookups, tx, datum) =
      do
        logInfo $ "Found oracle with datum: " ++ show datum ++ "."
        ledgerTx <- submitTxConstraintsWith @ClientScript lookups tx
        awaitTxConfirmed $ txId ledgerTx
        logInfo $ "Transaction succesful: " ++ show (txId ledgerTx) ++ "."
        return $ Just datum
  in
    maybe notFound found
      =<< readOracleConstraints oracle


-- | Schema for reading the oracle.
type ClientSchema = Endpoint "read" ()


-- | Find an oracle on the blockchain.
findOracle :: Oracle                                                      -- ^ The oracle.
           -> Contract w s Text (Maybe (TxOutRef, ChainIndexTxOut, Data)) -- ^ Action for finding the oracle's UTxO and datum.
findOracle oracle@Oracle{..} =
  do
    utxos <-
      M.filter
        (\ScriptChainIndexTxOut{..} -> assetClassValueOf _ciTxOutValue datumToken == 1)
        <$> utxosAt (oracleAddress oracle)
    case M.toList utxos of
      [(oref, o@ScriptChainIndexTxOut{..})] -> fmap ((oref, o, ) . builtinDataToData . getDatum)
                                                 <$> either datumFromHash (return . Just) _ciTxOutDatum
      _                                     -> return Nothing


-- | Repeatedly read the oracle's datum from its endpoint.
runOracleClient :: Oracle                                                         -- ^ The oracle.
                -> Promise (Last (Either Text (Maybe Data))) ClientSchema Void () -- ^ The action for repeatedly reading the oracle.
runOracleClient oracle =
  let
    read' =
      handleEndpoint @"read"
        $ \input ->
          (tell . Last . Just) =<<
            case input of
              Right () -> runError $ readOracle oracle
              Left  e  -> return $ Left e
  in
    read' <> runOracleClient oracle

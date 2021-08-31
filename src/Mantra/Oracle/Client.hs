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
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}


module Mantra.Oracle.Client (
-- * Script and Schema
  ClientScript
, ClientSchema
-- * Enpoints
, readOracleConstraints
, readOracle
-- * Utilities
, runOracleClient
) where


import PlutusTx.Prelude hiding ((<>))

import Data.Monoid          (Last (..))
import Data.Text            (Text)
import Ledger               (Datum(..), Redeemer(..), Value, txId, txOutTxOut, txOutValue, validatorHash)
import Ledger.Constraints   (ScriptLookups, TxConstraints, mustPayToOtherScript, mustSpendScriptOutput, otherScript, unspentOutputs)
import Ledger.Typed.Scripts (DatumType, RedeemerType, ScriptType)
import Mantra.Oracle        (findOracle, oracleValidator)
import Mantra.Oracle.Types  (Oracle(..), Action(Read))
import Plutus.Contract      (BlockchainActions, Contract, Endpoint, HasBlockchainActions, type (.\/), awaitTxConfirmed, endpoint, handleError, logError, logInfo, submitTxConstraintsWith)
import PlutusTx             (Data, toData)
import Prelude              (String, (<>), show)

import qualified Data.Map as M (singleton)


-- | Oracle script for clients.
data ClientScript

instance ScriptType ClientScript where
  type instance DatumType    ClientScript = ()
  type instance RedeemerType ClientScript = ()


-- | Compute the lookup and constratins for reading the oracle on-chain.
readOracleConstraints :: HasBlockchainActions s
                      => Oracle                                                               -- ^ The oracle.
                      -> Contract w s Text (Maybe (ScriptLookups a, TxConstraints i o, Data)) -- ^ Action for computing the oracle's lookups, constraints, and data.
readOracleConstraints oracle@Oracle{..} =
  let
    found (outputRef, output, datum) =
      let
         lookups = otherScript (oracleValidator oracle)
                <> unspentOutputs (M.singleton outputRef output)
         tx = mustSpendScriptOutput
                outputRef (Redeemer $ toData Read)
           <> mustPayToOtherScript
                (validatorHash $ oracleValidator oracle)
                (Datum $ toData datum)
                (txOutValue (txOutTxOut output) <> requiredFee)
      in
        (lookups, tx, datum)
  in
    do
      inst <- findOracle oracle
      return $ found <$> inst


-- | Endpoint for reading the datum from the oracle.
readOracle :: HasBlockchainActions s
           => Oracle                         -- ^ The oracle.
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
type ClientSchema = BlockchainActions
                .\/ Endpoint "read" ()


-- | Repeatedly read the oracle's datum from its endpoint.
runOracleClient :: Oracle                                     -- ^ The oracle.
                -> Contract (Last Value) ClientSchema Text () -- ^ The action for repeatedly reading the oracle.
runOracleClient oracle =
  let
    read' =
      handleError logError
        $  endpoint @"read"
        >> readOracle oracle
        >> return ()
  in
    read' >> runOracleClient oracle

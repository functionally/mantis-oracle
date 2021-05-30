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
-- | Controlling the general-purpose oracle.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


module Mantis.Oracle.Controller (
-- * Schema
  OracleSchema
-- * Endpoints
, createOracle
, deleteOracle
, writeOracle
-- * Utilities
, runOracleController
) where


import PlutusTx.Prelude hiding ((<>))

import Data.Monoid          (Last (..))
import Data.Text            (Text)
import Ledger               (Redeemer(..), pubKeyHash, txId)
import Ledger.Constraints   (mustPayToPubKey, mustPayToTheScript, mustSpendScriptOutput, otherScript, scriptInstanceLookups, unspentOutputs)
import Ledger.Value         (assetClassValue)
import Mantis.Oracle        (OracleScript, findOracle, oracleInstance, oracleValidator)
import Mantis.Oracle.Client (readOracle)
import Mantis.Oracle.Types  (Action(..), Oracle(..), Parameters, makeOracle)
import Plutus.Contract      (BlockchainActions, Contract, Endpoint, HasBlockchainActions, type (.\/), awaitTxConfirmed, endpoint, logError, logInfo, ownPubKey, select, submitTxConstraints, submitTxConstraintsWith, tell)
import PlutusTx             (Data, toData)
import Prelude              ((<>))

import qualified Data.Map as M (singleton)


-- | Create an oracle.
createOracle :: HasBlockchainActions s
             => Parameters               -- ^ Parameters defining the oracle.
             -> Contract w s Text Oracle -- ^ Action for creating the oracle.
createOracle parameters =
  do
    let
      oracle = makeOracle parameters
    logInfo $ "Created oracle: " ++ show oracle
    return oracle


-- | Schema for controlling the oracle.
type OracleSchema =
      BlockchainActions
  .\/ Endpoint "read"   ()
  .\/ Endpoint "write"  Data
  .\/ Endpoint "delete" ()


-- | Endpoint for writing datum to the oracle.
writeOracle :: HasBlockchainActions s
            => Oracle               -- ^ The oracle.
            -> Data                 -- ^ The datum to be written.
            -> Contract w s Text () -- ^ Action for writing the datum to the oracle.
writeOracle oracle@Oracle{..} datum =
  do
    owner <- pubKeyHash <$> ownPubKey
    let
      mustControl  = mustPayToPubKey    owner $ assetClassValue controlToken 1
      mustUseDatum = mustPayToTheScript datum $ assetClassValue datumToken   1
      notFound = 
        do
          ledgerTx <-
            submitTxConstraints (oracleInstance oracle)
              $ mustControl <> mustUseDatum
          awaitTxConfirmed $ txId ledgerTx
          logInfo $ "Set oracle datum: " ++ show datum ++ "."
      found (outputRef, output, _) =
        do
          let
            lookups = otherScript (oracleValidator oracle)
                   <> unspentOutputs (M.singleton outputRef output)
                   <> scriptInstanceLookups (oracleInstance oracle)
            tx = mustControl
              <> mustUseDatum
              <> mustSpendScriptOutput outputRef (Redeemer $ toData Write)
          ledgerTx <- submitTxConstraintsWith @OracleScript lookups tx
          awaitTxConfirmed $ txId ledgerTx
          logInfo $ "Updated oracle datum: " ++ show datum ++ "."
    maybe notFound found
      =<< findOracle oracle


-- | Endpoint for deleting (closing) the oracle.
deleteOracle :: HasBlockchainActions s
             => Oracle               -- ^ The oracle.
             -> Contract w s Text () -- ^ Action to close the oracle.
deleteOracle oracle@Oracle{..} =
  do
    owner <- pubKeyHash <$> ownPubKey
    let
      mustControl  = mustPayToPubKey    owner $ assetClassValue controlToken 1
      notFound = logError @String $ "Oracle not found."
      found (outputRef, output, _) =
        do
          let
            lookups = otherScript (oracleValidator oracle)
                   <> unspentOutputs (M.singleton outputRef output)
                   <> scriptInstanceLookups (oracleInstance oracle)
            tx = mustControl
              <> mustSpendScriptOutput outputRef (Redeemer $ toData Delete)
          ledgerTx <- submitTxConstraintsWith @OracleScript lookups tx
          awaitTxConfirmed $ txId ledgerTx
          logInfo @String $ "Deleted oracle datum."
    maybe notFound found
      =<< findOracle oracle


-- | Create the oracle and run its control endpoints.
runOracleController :: Parameters                                  -- ^ The oracle's parameters.
                    -> Contract (Last Oracle) OracleSchema Text () -- ^ Action for creating and running the oracle.
runOracleController parameters =
  do
    oracle <- createOracle parameters
    tell . Last $ Just oracle
    let
      write'  = endpoint @"write"  >>= writeOracle  oracle
      read'   = endpoint @"read"   >>  readOracle   oracle >> return ()
      delete' = endpoint @"delete" >>  deleteOracle oracle
      go = (write' `select` read' `select` delete') >> go
    go

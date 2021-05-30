{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


module Mantis.Oracle.Controller (
  OracleSchema
, createOracle
, deleteOracle
, writeOracle
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


createOracle :: HasBlockchainActions s
             => Parameters
             -> Contract w s Text Oracle
createOracle parameters =
  do
    let
      oracle = makeOracle parameters
    logInfo $ "Created oracle: " ++ show oracle
    return oracle


type OracleSchema =
      BlockchainActions
  .\/ Endpoint "read"   ()
  .\/ Endpoint "write"  Data
  .\/ Endpoint "delete" ()


writeOracle :: HasBlockchainActions s
            => Oracle
            -> Data
            -> Contract w s Text ()
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


deleteOracle :: HasBlockchainActions s
             => Oracle
             -> Contract w s Text ()
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


runOracleController :: Parameters
                    -> Contract (Last Oracle) OracleSchema Text ()
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

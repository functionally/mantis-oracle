{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


module Mantis.Oracle.Owner (
  OracleSchema
, createOracle
, deleteOracle
, writeOracle
, runOracleOwner
) where


import PlutusTx.Prelude hiding ((<>))

import Data.Monoid         (Last (..))
import Data.Text           (Text)
import Ledger              (Redeemer(..), pubKeyHash, txId)
import Ledger.Constraints  (mustPayToTheScript, mustSpendScriptOutput, otherScript, scriptInstanceLookups, unspentOutputs)
import Ledger.Value        (assetClassValue)
import Mantis.Oracle       (OracleScript, findOracle, oracleInstance, oracleValidator)
import Mantis.Oracle.Types (Action(..), Oracle(..), Parameters(..))
import Plutus.Contract     (BlockchainActions, Contract, Endpoint, HasBlockchainActions, type (.\/), awaitTxConfirmed, endpoint, logError, logInfo, ownPubKey, select, submitTxConstraints, submitTxConstraintsWith, tell)
import PlutusTx            (toData)
import Prelude             ((<>))

import qualified Data.Map as M (singleton)


createOracle :: HasBlockchainActions s
             => Parameters
             -> Contract w s Text Oracle
createOracle Parameters{..} =
  do
    owner <- pubKeyHash <$> ownPubKey
    let
      token = oracleToken
      fee = oracleFee
      oracle = Oracle{..}
    logInfo @String $ "Created oracle: " ++ show oracle
    return oracle


deleteOracle :: HasBlockchainActions s
             => Oracle
             -> Contract w s Text ()
deleteOracle oracle =
  let
    notFound = logError @String $ "Oracle not found."
    found (outputRef, output, _) =
      do
        let
          lookups = otherScript (oracleValidator oracle)
                 <> unspentOutputs (M.singleton outputRef output)
                 <> scriptInstanceLookups (oracleInstance oracle)
          tx = mustSpendScriptOutput outputRef (Redeemer $ toData Delete)
        ledgerTx <- submitTxConstraintsWith @OracleScript lookups tx
        awaitTxConfirmed $ txId ledgerTx
        logInfo @String $ "Deleted oracle datum."
  in
    maybe notFound found
      =<< findOracle oracle


type OracleSchema =
      BlockchainActions
  .\/ Endpoint "write"  Integer
  .\/ Endpoint "delete" ()


writeOracle :: HasBlockchainActions s
            => Oracle
            -> Integer
            -> Contract w s Text ()
writeOracle oracle@Oracle{..} datum =
  let
    mustUseToken = mustPayToTheScript datum $ assetClassValue token 1
    notFound = 
      do
        ledgerTx <- submitTxConstraints (oracleInstance oracle) mustUseToken
        awaitTxConfirmed $ txId ledgerTx
        logInfo @String $ "Set oracle datum: " ++ show datum ++ "."
    found (outputRef, output, _) =
      do
        let
          lookups = otherScript (oracleValidator oracle)
                 <> unspentOutputs (M.singleton outputRef output)
                 <> scriptInstanceLookups (oracleInstance oracle)
          tx = mustUseToken
            <> mustSpendScriptOutput outputRef (Redeemer $ toData Write)
        ledgerTx <- submitTxConstraintsWith @OracleScript lookups tx
        awaitTxConfirmed $ txId ledgerTx
        logInfo @String $ "Updated oracle datum: " ++ show datum ++ "."
  in
    maybe notFound found
      =<< findOracle oracle


runOracleOwner :: Parameters
         -> Contract (Last Oracle) OracleSchema Text ()
runOracleOwner parameters =
  do
    oracle <- createOracle parameters
    tell . Last $ Just oracle
    let
      write'  = endpoint @"write"  >>= writeOracle  oracle
      delete' = endpoint @"delete" >>  deleteOracle oracle
      go = (write' `select` delete') >> go
    go

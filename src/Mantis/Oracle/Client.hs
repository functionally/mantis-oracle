{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}


module Mantis.Oracle.Client (
  Client
, readOracleConstraints
, readOracle
, runOracleClient
) where


import PlutusTx.Prelude hiding ((<>))

import Data.Monoid          (Last (..))
import Data.Text            (Text)
import Ledger               (Datum(..), Redeemer(..), Value, txId, txOutTxOut, txOutValue, validatorHash)
import Ledger.Constraints   (ScriptLookups, TxConstraints, mustPayToOtherScript, mustSpendScriptOutput, otherScript, unspentOutputs)
import Ledger.Typed.Scripts (DatumType, RedeemerType, ScriptType)
import Mantis.Oracle        (findOracle, oracleValidator)
import Mantis.Oracle.Types  (Oracle(..), Action(Read))
import Plutus.Contract      (BlockchainActions, Contract, Endpoint, HasBlockchainActions, type (.\/), awaitTxConfirmed, endpoint, handleError, logError, logInfo, submitTxConstraintsWith)
import PlutusTx             (Data, toData)
import Prelude              ((<>))

import qualified Data.Map as M (singleton)


data Client

instance ScriptType Client where
  type instance DatumType    Client = ()
  type instance RedeemerType Client = ()


readOracleConstraints :: HasBlockchainActions s
                      => Oracle
                      -> Contract w s Text (Maybe (ScriptLookups a, TxConstraints i o, Data))
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


readOracle :: HasBlockchainActions s
           => Oracle
           -> Contract w s Text (Maybe Data)
readOracle oracle =
  let
    notFound =
      do
        logError @String "Oracle not found."
        return Nothing
    found (lookups, tx, datum) =
      do
        logInfo $ "Found oracle with datum: " ++ show datum ++ "."
        ledgerTx <- submitTxConstraintsWith @Client lookups tx
        awaitTxConfirmed $ txId ledgerTx
        logInfo $ "Transaction succesful: " ++ show (txId ledgerTx) ++ "."
        return $ Just datum
  in
    maybe notFound found
      =<< readOracleConstraints oracle


type ClientSchema = BlockchainActions
                .\/ Endpoint "read" ()


runOracleClient :: Oracle
                -> Contract (Last Value) ClientSchema Text ()
runOracleClient oracle =
  let
    read' =
      handleError logError
        $  endpoint @"read"
        >> readOracle oracle
        >> return ()
  in
    read' >> runOracleClient oracle

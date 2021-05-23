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
, readOracle
, runOracleClient
) where


import PlutusTx.Prelude hiding ((<>))

import Data.Monoid          (Last (..))
import Data.Text            (Text)
import Ledger               (Datum(..), Redeemer(..), Value, txId, txOutTxOut, txOutValue, validatorHash)
import Ledger.Ada           (lovelaceValueOf)
import Ledger.Constraints   (mustPayToOtherScript, mustSpendScriptOutput, otherScript, unspentOutputs)
import Ledger.Typed.Scripts (DatumType, RedeemerType, ScriptType)
import Mantis.Oracle        (findOracle, oracleValidator)
import Mantis.Oracle.Types  (Oracle(..), Action(Read))
import Plutus.Contract      (BlockchainActions, Contract, Endpoint, HasBlockchainActions, type (.\/), awaitTxConfirmed, endpoint, handleError, logError, logInfo, submitTxConstraintsWith)
import PlutusTx             (toData)
import Prelude              ((<>))

import qualified Data.Map as M (singleton)


data Client

instance ScriptType Client where
  type instance DatumType    Client = ()
  type instance RedeemerType Client = ()


readOracle :: HasBlockchainActions s
           => Oracle
           -> Contract w s Text ()
readOracle oracle@Oracle{..} =
  let
    notFound = logError @String "Oracle not found."
    found (outputRef, output, datum) =
      do
        logInfo @String $ "Found oracle with datum:  " ++ show datum ++ "."
        let
           lookups = otherScript (oracleValidator oracle)
                  <> unspentOutputs (M.singleton outputRef output)
           tx = mustSpendScriptOutput
                  outputRef (Redeemer $ toData Read)
             <> mustPayToOtherScript
                  (validatorHash $ oracleValidator oracle)
                  (Datum $ toData datum)
                  (txOutValue (txOutTxOut output) <> lovelaceValueOf fee)
        ledgerTx <- submitTxConstraintsWith @Client lookups tx
        awaitTxConfirmed $ txId ledgerTx
        logInfo @String $ "Transaction succesful: " ++ show (txId ledgerTx) ++ "."
  in
    maybe notFound found
      =<< findOracle oracle


type ClientSchema = BlockchainActions
                .\/ Endpoint "read" ()


runOracleClient :: Oracle ->
                   Contract (Last Value) ClientSchema Text ()
runOracleClient oracle =
  let
    read' =
      handleError logError
        $  endpoint @"read"
        >> readOracle oracle
  in
    read' >> runOracleClient oracle

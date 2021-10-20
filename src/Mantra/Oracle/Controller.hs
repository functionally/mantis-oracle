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


module Mantra.Oracle.Controller (
-- * Schema
  OracleSchema
-- * Endpoints
, deleteOracle
, writeOracle
-- * Utilities
, runOracleController
) where


import PlutusTx.Prelude hiding ((<>))

import Data.Monoid          (Last (..))
import Data.Text            (Text)
import Data.Void            (Void)
import Ledger               (Redeemer(..), pubKeyHash, txId)
import Ledger.Constraints   (mustPayToPubKey, mustPayToTheScript, mustSpendScriptOutput, otherScript, typedValidatorLookups, unspentOutputs)
import Ledger.Value         (assetClassValue)
import Mantra.Oracle        (OracleScript, oracleInstance, oracleValidator)
import Mantra.Oracle.Client (findOracle, readOracle)
import Mantra.Oracle.Types  (Action(..), Oracle(..))
import Plutus.Contract      (Contract, Endpoint, Promise, type (.\/), awaitTxConfirmed, handleEndpoint, logError, logInfo, ownPubKey, runError, select, submitTxConstraints, submitTxConstraintsWith, tell)
import PlutusTx             (Data, ToData(..), dataToBuiltinData)
import Prelude              (String, (<>), show)

import qualified Data.Map as M (singleton)


-- | Schema for controlling the oracle.
type OracleSchema =
      Endpoint "read"   ()
  .\/ Endpoint "write"  Data
  .\/ Endpoint "delete" ()


-- | Endpoint for writing datum to the oracle.
writeOracle :: Oracle               -- ^ The oracle.
            -> Data                 -- ^ The datum to be written.
            -> Contract w s Text () -- ^ Action for writing the datum to the oracle.
writeOracle oracle@Oracle{..} datum =
  do
    owner <- pubKeyHash <$> ownPubKey
    let
      mustControl  = mustPayToPubKey    owner                     $ assetClassValue controlToken 1
      mustUseDatum = mustPayToTheScript (dataToBuiltinData datum) $ assetClassValue datumToken   1
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
                   <> typedValidatorLookups (oracleInstance oracle)
            tx = mustControl
              <> mustUseDatum
              <> mustSpendScriptOutput outputRef (Redeemer $ toBuiltinData Write)
          ledgerTx <- submitTxConstraintsWith @OracleScript lookups tx
          awaitTxConfirmed $ txId ledgerTx
          logInfo $ "Updated oracle datum: " ++ show datum ++ "."
    maybe notFound found
      =<< findOracle oracle


-- | Endpoint for deleting (closing) the oracle.
deleteOracle :: Oracle               -- ^ The oracle.
             -> Contract w s Text () -- ^ Action to close the oracle.
deleteOracle oracle@Oracle{..} =
  do
    owner <- pubKeyHash <$> ownPubKey
    let
      mustControl = mustPayToPubKey owner $ assetClassValue controlToken 1
      notFound = logError @String $ "Oracle not found."
      found (outputRef, output, _) =
        do
          let
            lookups = otherScript (oracleValidator oracle)
                   <> unspentOutputs (M.singleton outputRef output)
                   <> typedValidatorLookups (oracleInstance oracle)
            tx = mustControl
              <> mustSpendScriptOutput outputRef (Redeemer $ toBuiltinData Delete)
          ledgerTx <- submitTxConstraintsWith @OracleScript lookups tx
          awaitTxConfirmed $ txId ledgerTx
          logInfo @String $ "Deleted oracle datum."
    maybe notFound found
      =<< findOracle oracle


-- | Create the oracle and run its control endpoints.
runOracleController :: Oracle                                                   -- ^ The oracle.
                    -> Promise (Last (Either Text Oracle)) OracleSchema Void () -- ^ Action for creating and running the oracle.
runOracleController oracle =
  do
    let
      write' =
        handleEndpoint @"write"
          $ \input ->
            (tell . Last . Just) =<<
              case input of
                Right datum -> fmap (either Left (const $ Right oracle))
                                 . runError
                                 $ writeOracle oracle datum
                Left  e     -> return $ Left e
      read' =
        handleEndpoint @"read"
          $ \input ->
            (tell . Last . Just) =<<
              case input of
                Right () -> fmap (either Left (const $ Right oracle))
                              . runError
                              $ readOracle oracle
                Left  e  -> return $ Left e
      delete' =
        handleEndpoint @"delete"
          $ \input ->
            (tell . Last . Just) =<<
              case input of
                Right () -> fmap (either Left (const $ Right oracle))
                              . runError
                              $ deleteOracle oracle
                Left  e  -> return $ Left e
    let
      operate = (read' `select` write' `select` delete') <> operate
    operate

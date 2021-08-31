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
-- | Plutus application backend simulation for oracle contracts.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}


module Mantra.Oracle.Simulate.PAB (
-- * Types
  TheContracts(..)
-- * Action
, runPAB
) where


import Control.Monad                       (forM_, void, when)
import Control.Monad.Freer                 (Eff, Member, interpret, type (~>))
import Control.Monad.Freer.Error           (Error)
import Control.Monad.Freer.Extras.Log      (LogMsg)
import Control.Monad.IO.Class              (MonadIO (..))
import Data.Aeson                          (FromJSON, Result(..), ToJSON, fromJSON)
import Data.Monoid                         (Last (..))
import Data.Text                           (Text, pack)
import Data.Text.Prettyprint.Doc           (Pretty (..), viaShow)
import GHC.Generics                        (Generic)
import Ledger                              (pubKeyHash, txId)
import Ledger.Constraints                  (mustPayToPubKey)
import Ledger.Value                        (AssetClass(..), TokenName, singleton)
import Mantra.Orphans                      ()
import Plutus.Contract                     (BlockchainActions, Contract, ContractInstanceId, Empty, awaitTxConfirmed, mapError, ownPubKey, submitTx, tell)
import Plutus.PAB.Effects.Contract         (ContractEffect (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), type (.\\), endpointsToSchemas, handleBuiltin)
import Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import Plutus.PAB.Types                    (PABError (..))
import Wallet.Emulator.Types               (Wallet (..), walletPubKey)
import Wallet.Types                        (ContractInstanceId (..))

import qualified Mantra.Oracle.Client        as O (ClientSchema, runOracleClient)
import qualified Mantra.Oracle.Controller    as O (OracleSchema, runOracleController)
import qualified Mantra.Oracle.Types         as O (Oracle, Parameters(..))
import qualified Plutus.Contracts.Currency   as C (CurrencyError, OneShotCurrency, currencySymbol, forgeContract)
import qualified Plutus.PAB.Simulator        as S (Simulation, activateContract, logString, mkSimulatorHandlers, runSimulationWith, waitForState, waitUntilFinished)
import qualified Plutus.PAB.Webserver.Server as P (startServerDebug)


-- | Contract specifications.
data TheContracts =
    Init TokenName TokenName TokenName Integer [(Wallet, Integer, FilePath)] -- ^ Initializer with control token name, datum token name, fee token name, fee amount, and wallets with initial fee tokens and CID path.
  | TheController O.Parameters                                               -- ^ Controller.
  | TheClient     O.Oracle                                                   -- ^ Client.
    deriving (Eq, FromJSON, Generic, Show, ToJSON)

instance Pretty TheContracts where
  pretty = viaShow


-- | Run the Plutus application backend. The first wallet controls the oracle.
runPAB :: TokenName                     -- ^ Name for the control token.
       -> TokenName                     -- ^ Name for the datum token.
       -> TokenName                     -- ^ Name for the fee token.
       -> Integer                       -- ^ The fee amount.
       -> [(Wallet, Integer, FilePath)] -- ^ Wallets, their initial fee token amount, and the paths to their Contract ID (CID) files.
       -> IO ()                         -- ^ Action for running the PAB.
runPAB controlName datumName feeName feeAmount wallets =
  void . S.runSimulationWith handlers
    $ do

      let
        (w1, _, f1) = head wallets

      S.logString @(Builtin TheContracts) "Starting Oracle PAB webserver. Press enter to exit."
      shutdown <- P.startServerDebug

      cidInit <-
        S.activateContract w1
          $ Init controlName datumName feeName feeAmount wallets
      parameters <- waitForLast cidInit
      void $ S.waitUntilFinished cidInit

      cidOracle <- S.activateContract w1 $ TheController parameters
      liftIO . writeFile f1 . show $ unContractInstanceId cidOracle
      oracle <- waitForLast cidOracle

      forM_ wallets
        $ \(w, _, f) ->
          when (w /= w1)
            $ do
              cid <- S.activateContract w $ TheClient oracle
              liftIO
                . writeFile f
                . show
                $ unContractInstanceId cid

      void $ liftIO getLine
      shutdown


-- | Wait for the success of a contract.
waitForLast :: FromJSON a
            => ContractInstanceId -- ^ The Contract ID.
            -> S.Simulation t a   -- ^ Action for waiting.
waitForLast cid =
  flip S.waitForState cid
    $ \json ->
      case fromJSON json of
        Success (Last (Just x)) -> Just x
        _                       -> Nothing


-- | Handle the contracts.
handleTheContracts :: Member (Error PABError) effs
                   => Member (LogMsg (PABMultiAgentMsg (Builtin TheContracts))) effs
                   => ContractEffect (Builtin TheContracts)
                   ~> Eff effs
handleTheContracts =
  let
    getSchema = \case
      Init{}          -> endpointsToSchemas @Empty
      TheController _ -> endpointsToSchemas @(O.OracleSchema .\\ BlockchainActions)
      TheClient     _ -> endpointsToSchemas @(O.ClientSchema .\\ BlockchainActions)
    getContract = \case
      Init controlName datumName feeName feeAmount wallets
        -> SomeBuiltin $ initContract controlName datumName feeName feeAmount wallets
      TheController parameters -> SomeBuiltin $ O.runOracleController parameters
      TheClient oracle         -> SomeBuiltin $ O.runOracleClient oracle
  in
    handleBuiltin getSchema getContract


-- | Handlers for the contract.
handlers :: SimulatorEffectHandlers (Builtin TheContracts)
handlers =
  S.mkSimulatorHandlers @(Builtin TheContracts) []
    $ interpret handleTheContracts


-- | Initialize the contract. The first wallet controls the oracle.
initContract :: TokenName                                              -- ^ Name for the control token.
             -> TokenName                                              -- ^ Name for the datum token.
             -> TokenName                                              -- ^ Name for the fee token.
             -> Integer                                                -- ^ The fee amount.
             -> [(Wallet, Integer, FilePath)]                          -- ^ Wallets, their initial fee token amount, and the paths to their Contract ID (CID) files.
             -> Contract (Last O.Parameters) BlockchainActions Text () -- ^ Action for initializing the contract.
initContract controlName datumName feeName feeAmount wallets =
  do
    ownPK <- pubKeyHash <$> ownPubKey
    symbol <-
      mapError (pack . show)
       $ C.currencySymbol
       <$> (
             C.forgeContract ownPK
               [
                 (controlName, 1                     )
               , (datumName  , 1                     )
               , (feeName    , sum $ snd3 <$> wallets)
               ]
             :: Contract (Last O.Parameters) BlockchainActions C.CurrencyError C.OneShotCurrency
           )
    sequence_
      [
        do
          let
            pkh = pubKeyHash $ walletPubKey w
            v = singleton symbol feeName amount
          tx <- submitTx $ mustPayToPubKey pkh v
          awaitTxConfirmed $ txId tx
      |
         (w, amount, _) <- wallets
      ]
    let
      controlParameter = AssetClass (symbol, controlName)
      datumParameter   = AssetClass (symbol, datumName  )
      feeToken         = AssetClass (symbol, feeName    )
    tell . Last $ Just O.Parameters{..}


-- | Extract the second entry in a triplet.
snd3 :: (a, b, c) -> b
snd3 (_, x, _) = x

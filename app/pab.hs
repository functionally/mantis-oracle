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
-- | Plutus application backend for oracle contracts.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE TypeOperators      #-}


module Main (
  main
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
import Ledger.Value                        (AssetClass(..), singleton)
import Orphans                             ()
import Plutus.Contract                     (BlockchainActions, Contract, ContractInstanceId, Empty, awaitTxConfirmed, mapError, ownPubKey, submitTx, tell)
import Plutus.PAB.Effects.Contract         (ContractEffect (..))
import Plutus.PAB.Effects.Contract.Builtin (Builtin, SomeBuiltin (..), type (.\\), endpointsToSchemas, handleBuiltin)
import Plutus.PAB.Monitoring.PABLogMsg     (PABMultiAgentMsg)
import Plutus.PAB.Simulator                (SimulatorEffectHandlers)
import Plutus.PAB.Types                    (PABError (..))
import Wallet.Emulator.Types               (Wallet (..), walletPubKey)
import Wallet.Types                        (ContractInstanceId (..))

import qualified Mantis.Oracle.Client        as O (ClientSchema, runOracleClient)
import qualified Mantis.Oracle.Controller    as O (OracleSchema, runOracleController)
import qualified Mantis.Oracle.Types         as O (Oracle, Parameters(..))
import qualified Plutus.Contracts.Currency   as C (CurrencyError, OneShotCurrency, currencySymbol, forgeContract)
import qualified Plutus.PAB.Simulator        as S (Simulation, activateContract, logString, mkSimulatorHandlers, runSimulationWith, waitForState, waitUntilFinished)
import qualified Plutus.PAB.Webserver.Server as P (startServerDebug)


data TheContracts =
    Init
  | TheController O.Parameters
  | TheClient     O.Oracle
    deriving (Eq, FromJSON, Generic, Show, ToJSON)

instance Pretty TheContracts where
  pretty = viaShow


main :: IO ()
main =
  void . S.runSimulationWith handlers
    $ do

      S.logString @(Builtin TheContracts) "Starting Oracle PAB webserver. Press enter to exit."
      shutdown <- P.startServerDebug

      cidInit <- S.activateContract (Wallet 1) Init
      parameters <- waitForLast cidInit
      void $ S.waitUntilFinished cidInit

      cidOracle <- S.activateContract (Wallet 1) $ TheController parameters
      liftIO . writeFile "oracle.cid" . show $ unContractInstanceId cidOracle
      oracle <- waitForLast cidOracle

      forM_ wallets
        $ \w ->
          when (w /= Wallet 1)
            $ do
              cid <- S.activateContract w $ TheClient oracle
              liftIO
                . writeFile ('W' : show (getWallet w) ++ ".cid")
                . show
                $ unContractInstanceId cid

      void $ liftIO getLine
      shutdown


waitForLast :: FromJSON a
            => ContractInstanceId
            -> S.Simulation t a
waitForLast cid =
  flip S.waitForState cid
    $ \json ->
      case fromJSON json of
        Success (Last (Just x)) -> Just x
        _                       -> Nothing


wallets :: [Wallet]
wallets = [Wallet i | i <- [1 .. 3]]


handleTheContracts :: Member (Error PABError) effs
                   => Member (LogMsg (PABMultiAgentMsg (Builtin TheContracts))) effs
                   => ContractEffect (Builtin TheContracts)
                   ~> Eff effs
handleTheContracts =
  let
    getSchema = \case
      Init            -> endpointsToSchemas @Empty
      TheController _ -> endpointsToSchemas @(O.OracleSchema .\\ BlockchainActions)
      TheClient     _ -> endpointsToSchemas @(O.ClientSchema .\\ BlockchainActions)
    getContract = \case
      Init                     -> SomeBuiltin initContract
      TheController parameters -> SomeBuiltin $ O.runOracleController parameters
      TheClient oracle         -> SomeBuiltin $ O.runOracleClient oracle
  in
    handleBuiltin getSchema getContract


handlers :: SimulatorEffectHandlers (Builtin TheContracts)
handlers =
  S.mkSimulatorHandlers @(Builtin TheContracts) []
    $ interpret handleTheContracts


initContract :: Contract (Last O.Parameters) BlockchainActions Text ()
initContract =
  do
    let
      controlName = "BRIO"
      datumName   = "SOFR"
      feeName     = "PIGY"
      feeAmount   = 1_000_000
    ownPK <- pubKeyHash <$> ownPubKey
    symbol <-
      mapError (pack . show)
       $ C.currencySymbol
       <$> (
             C.forgeContract ownPK [(controlName, 1), (datumName, 1), (feeName, 5 * feeAmount)]
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
         (w, amount) <- [
                              (wallets!! 1, 4_500_000)
                            , (wallets!! 2, 0_500_000)
                            ]
      ]
    let
      controlParameter = AssetClass (symbol, controlName)
      datumParameter   = AssetClass (symbol, datumName  )
      feeToken         = AssetClass (symbol, feeName    )
    tell . Last $ Just O.Parameters{..}

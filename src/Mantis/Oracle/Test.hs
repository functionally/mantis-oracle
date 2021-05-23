{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}


module Mantis.Oracle.Test (
  test
) where


import PlutusTx.Prelude hiding ((<>), last)

import Control.Monad          (void)
import Data.Default           (def)
import Data.Monoid            (Last (..))
import Data.Text              (Text)
import Ledger                 (CurrencySymbol, TokenName)
import Ledger.Ada             (lovelaceValueOf)
import Ledger.Value           (AssetClass(..))
import Mantis.Oracle          (findOracle)
import Mantis.Oracle.Client   (runOracleClient)
import Mantis.Oracle.Owner    (runOracleOwner)
import Mantis.Oracle.Types    (Oracle, Parameters(..))
import Prelude                ((<>))
import Wallet.Emulator.Wallet (Wallet(..))

import qualified Control.Monad.Freer.Extras as X (logInfo)
import qualified Data.Map                   as M (fromList)
import qualified Ledger.Value               as V (singleton)
import qualified Plutus.Contract            as C (BlockchainActions, Contract, logInfo, waitNSlots)
import qualified Plutus.Trace.Emulator      as E (EmulatorConfig(..), EmulatorTrace, activateContractWallet, callEndpoint, observableState, runEmulatorTraceIO', waitNSlots)


test :: IO ()
test =
  let
    initial = lovelaceValueOf 100_000_000
    initial' = initial <> V.singleton oracleSymbol oracleName 1
    configuration =
      E.EmulatorConfig
        . Left
        .  M.fromList
        $  (Wallet 1, initial')
        : [(Wallet i, initial ) | i <- [2 .. 3]]
  in
    E.runEmulatorTraceIO' def configuration testTrace


testTrace :: E.EmulatorTrace ()
testTrace =
  do
    let
      parameters =
        Parameters
        {
          oracleToken = AssetClass (oracleSymbol, oracleName)
        , oracleFee   = 2_000_000
        }
      getOracle w =
        do
          last <- E.observableState w
          case last of
            Last Nothing       -> E.waitNSlots 1        >> getOracle w
            Last (Just oracle) -> X.logInfo (show oracle) >> return oracle

    w1 <- E.activateContractWallet (Wallet 1)
       $  runOracleOwner parameters
    void $ E.waitNSlots 1
    oracle <- getOracle w1

    void . E.activateContractWallet (Wallet 2)
         $ peekDatum oracle

    E.callEndpoint @"write" w1 1_500
    void $ E.waitNSlots 3

    w3 <- E.activateContractWallet (Wallet 3) 
       $  runOracleClient oracle

    E.callEndpoint @"read" w3 ()
    void $ E.waitNSlots 3

    E.callEndpoint @"write" w1 2_500
    void $ E.waitNSlots 3

    E.callEndpoint @"read" w3 ()
    void $ E.waitNSlots 3


peekDatum :: Oracle
          -> C.Contract () C.BlockchainActions Text a
peekDatum oracle =
  do
    inst <- findOracle oracle
    case inst of
      Nothing            -> return ()
      Just (_, _, datum) -> C.logInfo $ "Oracle value: " ++ show datum ++ "."
    C.waitNSlots 1
      >> peekDatum oracle




oracleSymbol :: CurrencySymbol
oracleSymbol = "cafe"

oracleName :: TokenName
oracleName = "SOFR"

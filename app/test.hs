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
-- | Example usage for the oracle.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeApplications   #-}


module Main (
-- * Example
  main
) where


import PlutusTx.Prelude hiding ((<>))

import Control.Monad            (void)
import Data.Default             (def)
import Data.Monoid              (Last (..))
import Data.Text                (Text)
import Ledger.Ada               (fromValue, getLovelace, lovelaceValueOf)
import Ledger.Address           (pubKeyAddress)
import Ledger.Tx                (txOutTxOut, txOutValue)
import Ledger.Value             (AssetClass(..), Value, assetClass, assetClassValueOf, flattenValue, toString)
import Mantis.Oracle            (findOracle, oracleAddress)
import Mantis.Oracle.Client     (runOracleClient)
import Mantis.Oracle.Controller (runOracleController)
import Mantis.Oracle.Types      (Oracle(..), Parameters(..))
import Prelude                  (IO, String, (<>), show)
import PlutusTx                 (Data(I))
import Wallet.Emulator.Wallet   (Wallet(..))

import qualified Control.Monad.Freer.Extras as X (logInfo)
import qualified Data.Map                   as M (elems, fromList)
import qualified Ledger.Value               as V (singleton)
import qualified Plutus.Contract            as C (BlockchainActions, Contract, handleError, logError, logInfo, ownPubKey, tell, utxoAt, waitNSlots)
import qualified Plutus.Trace.Emulator      as E (EmulatorConfig(..), EmulatorTrace, activateContractWallet, callEndpoint, observableState, runEmulatorTraceIO', waitNSlots)


-- | Run the example.
main :: IO () -- ^ Action for running the example.
main =

  let

    symbol = "addacafe"
    controlName = "BRIO"
    datumName   = "SOFR"
    feeName     = "PIGY"

    controlParameter = AssetClass (symbol, controlName)
    datumParameter   = AssetClass (symbol, datumName  )
    feeToken         = AssetClass (symbol, feeName    )
    feeAmount = 1_000_000
   
    initial = lovelaceValueOf 100_000_000
    withValue = V.singleton symbol

    config =
      E.EmulatorConfig
        . Left
        $  M.fromList
        [
          -- Wallet 1 controls the oracle.
          (
            Wallet 1
          , initial <> controlName `withValue` 1 <> datumName `withValue` 1
          )
        , (
          -- Wallet 2 has enough of the fee token to read the oracle twice.
            Wallet 2
          , initial <> feeName `withValue` 2_000_000
          )
        , (
            -- Wallet 3 doesn't not have have enough fee tokens to read the oracle at all.
            Wallet 3
          , initial <> feeName `withValue` 500_000
          )
        , (
            -- Wallet 4 just monitors transactions of the oracle.
            Wallet 4
          , initial
          )
        ]

  in

    E.runEmulatorTraceIO' def config
      $ testTrace Parameters{..}


-- | Trace the oracles operation for a given set of parameters.
testTrace :: Parameters         -- ^ The parameters for the oracle.
          -> E.EmulatorTrace () -- ^ The action for running the test.
testTrace parameters =
  do

    let
      getOracle w =
        do
          last <- E.observableState w
          case last of
            Last Nothing       -> E.waitNSlots 1          >> getOracle w
            Last (Just oracle) -> X.logInfo (show oracle) >> return oracle

    X.logInfo @String "Wallet 1 starts the oracle, but does not set its value yet."
    w1 <- E.activateContractWallet (Wallet 1)
       $  runOracleController parameters
    void $ E.waitNSlots 1
    oracle <- getOracle w1

    sequence_ [E.activateContractWallet (Wallet i) . peekFunds oracle $ Just i | i <- [1..4]]
    void . E.activateContractWallet (Wallet 1) $ peekFunds oracle Nothing

    X.logInfo @String "Wallet 4 just watches transactions of the oracle."
    void . E.activateContractWallet (Wallet 4)
         $ peekDatum oracle
    void $ E.waitNSlots 3

    X.logInfo @String "Wallet 2 has sufficient fee token to read the oracle twice."
    w2 <- E.activateContractWallet (Wallet 2) 
       $  runOracleClient oracle
    void $ E.waitNSlots 3

    X.logInfo @String "Wallet 2 tries to read the oracle before it holds data."
    E.callEndpoint @"read" w2 ()
    void $ E.waitNSlots 3

    X.logInfo @String "Wallet 1 writes a value to the oracle."
    E.callEndpoint @"write" w1 $ I 1_500
    void $ E.waitNSlots 3

    X.logInfo @String "Wallet 2 read the oracle."
    E.callEndpoint @"read" w2 ()
    void $ E.waitNSlots 3

    X.logInfo @String "Wallet 3 doesn't have sufficient fee token to read the oracle at all."
    w3 <- E.activateContractWallet (Wallet 3) 
       $  runOracleClient oracle
    void $ E.waitNSlots 3

    X.logInfo @String "Wallet 3 tries to read the oracle, even though it has insufficient fee tokens."
    E.callEndpoint @"read" w3 ()
    void $ E.waitNSlots 3

    X.logInfo @String "Wallet 1 updates the value of the oracle."
    E.callEndpoint @"write" w1 $ I 2_500
    void $ E.waitNSlots 3

    X.logInfo @String "Wallet 2 reads the oracle a second time."
    E.callEndpoint @"read" w2 ()
    void $ E.waitNSlots 3

    X.logInfo @String "Wallet 1 deletes the oracle."
    E.callEndpoint @"delete" w1 ()
    void $ E.waitNSlots 3

    X.logInfo @String "Wallet 2 tries to read the oracle after its deletion."
    E.callEndpoint @"read" w2 ()
    void $ E.waitNSlots 3


-- | Log the oracle's current data.
peekDatum :: Oracle                                    -- ^ The oracle.
          -> C.Contract () C.BlockchainActions Text () -- ^ Action for logging the oracle's data.
peekDatum oracle =
  do
    inst <- findOracle oracle
    case inst of
      Just (_, _, I datum) -> C.logInfo $ "Oracle value: " ++ show datum ++ "."
      _                    -> return ()
    C.waitNSlots 1
      >> peekDatum oracle


-- } Log the funds in a wallet or in the oracle script.
peekFunds :: Oracle                                              -- ^ The oracle.
          -> Maybe Integer                                       -- ^ The wallet number, or `Nothing` for the oracle script.
          -> C.Contract (Last Value) C.BlockchainActions Text () -- ^ Action for logging the funds.
peekFunds oracle@Oracle{..} i =
  do
    let
      [(currency, feeName, _)] = flattenValue requiredFee
      feeToken = assetClass currency feeName
      funds :: C.Contract (Last Value) C.BlockchainActions Text Value
      funds =
        do
          owner <- C.ownPubKey
          utxos <- C.utxoAt $ maybe (oracleAddress oracle) (const $ pubKeyAddress owner) i
          let
            value = sum $ txOutValue . txOutTxOut <$> M.elems utxos
          C.logInfo
            $ maybe "Funds in Script" (("Funds in Wallet " ++) . show) i
            ++ ": " ++ show (getLovelace $ fromValue value) ++ " Lovelace"
            ++ ", " ++ show (assetClassValueOf value controlToken) ++ " " ++ toString (snd $ unAssetClass controlToken)
            ++ ", " ++ show (assetClassValueOf value datumToken  ) ++ " " ++ toString (snd $ unAssetClass datumToken  )
            ++ ", " ++ show (assetClassValueOf value feeToken    ) ++ " " ++ toString feeName
          return value
    C.handleError C.logError
      $ funds >>= C.tell . Last . Just
    void $ C.waitNSlots 1
    peekFunds oracle i

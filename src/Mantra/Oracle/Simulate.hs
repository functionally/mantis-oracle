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


module Mantra.Oracle.Simulate (
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
import Ledger.Tx                (ChainIndexTxOut(..))
import Ledger.Value             (AssetClass(..), CurrencySymbol, TokenName, Value, assetClass, assetClassValueOf, flattenValue, toString)
import Mantra.Oracle            (oracleAddress)
import Mantra.Oracle.Client     (findOracle, runOracleClient)
import Mantra.Oracle.Controller (OracleSchema, runOracleController)
import Mantra.Oracle.Types      (Oracle(..), Parameters(..), makeOracle)
import Prelude                  (IO, String, (<>), div, show)
import PlutusTx                 (Data(I))
import Wallet.Emulator.Wallet   (Wallet(..))

import qualified Control.Monad.Freer.Extras as X (logInfo)
import qualified Data.Map                   as M (elems, fromList)
import qualified Ledger.Value               as V (singleton)
import qualified Plutus.Contract            as C (Contract, logInfo, ownPubKey, tell, utxosAt, waitNSlots)
import qualified Plutus.Contract.Test       as C (w1, w2, w3, w4)
import qualified Plutus.Trace.Emulator      as E (EmulatorConfig(..), EmulatorTrace, activateContractWallet, callEndpoint, runEmulatorTraceIO', waitNSlots)


-- | Run the example.
main :: CurrencySymbol -- ^ Currency symbol for the example tokens.
     -> TokenName      -- ^ Name of the control token.
     -> TokenName      -- ^ Name of the datum token.
     -> TokenName      -- ^ Name of the fee token.
     -> Integer        -- ^ Amount of fee token needed to read the oracle.
     -> Integer        -- ^ Amount of lovelace to read the oracle.
     -> IO ()          -- ^ Action for running the example.
main symbol controlName datumName feeName feeAmount lovelaceAmount =

  let

    controlParameter = AssetClass (symbol, controlName)
    datumParameter   = AssetClass (symbol, datumName  )
    feeToken         = AssetClass (symbol, feeName    )

    initial = lovelaceValueOf 100_000_000
    withValue = V.singleton symbol

    _initialChainState =
      Left
        $  M.fromList
        [
          -- Wallet 1 controls the oracle.
          (
            C.w1
          , initial <> controlName `withValue` 1 <> datumName `withValue` 1
          )
        , (
          -- Wallet 2 has enough of the fee token to read the oracle twice.
            C.w2
          , initial <> feeName `withValue` (feeAmount * 2)
          )
        , (
            -- Wallet 3 doesn't not have have enough fee tokens to read the oracle at all.
            C.w3
          , initial <> feeName `withValue` (feeAmount `div` 2)
          )
        , (
            -- Wallet 4 just monitors transactions of the oracle.
            C.w4
          , initial
          )
        ]
    _slotConfig = def
    _feeConfig = def

  in

   do
    E.runEmulatorTraceIO' def E.EmulatorConfig{..}
      $ testTrace Parameters{..}


-- | Trace the oracles operation for a given set of parameters.
testTrace :: Parameters         -- ^ The parameters for the oracle.
          -> E.EmulatorTrace () -- ^ The action for running the test.
testTrace parameters =
  do

    let
      oracle = makeOracle parameters

    X.logInfo @String "Wallet 1 starts the oracle, but does not set its value yet."
    w1 <- E.activateContractWallet C.w1
       $  runOracleController oracle
    void $ E.waitNSlots 1

    sequence_ [E.activateContractWallet w . peekFunds oracle $ Just w | w <- [C.w1, C.w2, C.w3, C.w4]]
    void . E.activateContractWallet C.w1 $ peekFunds oracle Nothing

    X.logInfo @String "Wallet 4 just watches transactions of the oracle."
    void . E.activateContractWallet C.w4
         $ peekDatum oracle
    void $ E.waitNSlots 3

    X.logInfo @String "Wallet 2 has sufficient fee token to read the oracle twice."
    w2 <- E.activateContractWallet C.w2
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
    w3 <- E.activateContractWallet C.w3
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
peekDatum :: Oracle                             -- ^ The oracle.
          -> C.Contract () OracleSchema Text () -- ^ Action for logging the oracle's data.
peekDatum oracle =
  do
    inst <- findOracle oracle
    case inst of
      Just (_, _, I datum) -> C.logInfo $ "Oracle value: " ++ show datum ++ "."
      _                    -> return ()
    C.waitNSlots 1
      >> peekDatum oracle


-- | Log the funds in a wallet or in the oracle script.
peekFunds :: Oracle                                       -- ^ The oracle.
          -> Maybe Wallet                                 -- ^ The wallet, or `Nothing` for the oracle script.
          -> C.Contract (Last Value) OracleSchema Text () -- ^ Action for logging the funds.
peekFunds oracle@Oracle{..} i =
  do
    let
      [(currency, feeName, _)] = flattenValue requiredFee
      feeToken = assetClass currency feeName
      funds =
        do
          owner <- C.ownPubKey
          utxos <- C.utxosAt $ maybe (oracleAddress oracle) (const $ pubKeyAddress owner) i
          let
            value = sum $ _ciTxOutValue <$> M.elems utxos
          C.logInfo
            $ maybe "Funds in Script" (("Funds in Wallet " ++) . show) i
            ++ ": " ++ show (getLovelace $ fromValue value) ++ " Lovelace"
            ++ ", " ++ show (assetClassValueOf value controlToken) ++ " " ++ toString (snd $ unAssetClass controlToken)
            ++ ", " ++ show (assetClassValueOf value datumToken  ) ++ " " ++ toString (snd $ unAssetClass datumToken  )
            ++ ", " ++ show (assetClassValueOf value feeToken    ) ++ " " ++ toString feeName
          return value
    funds >>= C.tell . Last . Just
    void $ C.waitNSlots 1
    peekFunds oracle i

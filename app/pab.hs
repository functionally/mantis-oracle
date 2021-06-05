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
-- | Example Plutus application backend simulation for oracle contracts.
--
-----------------------------------------------------------------------------


{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}


module Main (
-- * Entry point
  main
) where


import Mantis.Oracle.PAB     (runPAB)
import Wallet.Emulator.Types (Wallet (..))


-- | Run the PAB for an example oracle.
main :: IO () -- ^ Action to run the oracle PAB.
main =
  runPAB
    "BRIO"
    "SOFR"
    "PIGY"
    1_000_000
    [
      (Wallet 1,         0, "oracle.cid"  )
    , (Wallet 2, 5_500_000, "wallet-1.cid")
    , (Wallet 3, 4_500_000, "wallet-2.cid")
    ]

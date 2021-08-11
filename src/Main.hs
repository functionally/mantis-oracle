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
-- | Command-line for oracle testing and simulation.
--
-----------------------------------------------------------------------------


{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}


module Main (
-- * Commands
  main
) where


import Cardano.Api
import Ledger.Value
import Mantis.Oracle -- (exportOracle)
import Mantis.Oracle.Types

import qualified Data.ByteString.Base16 as Base16 (decode, encode)


main :: IO ()
main =
  do
    let
      Right currency = Base16.decode "13a3efd825703a352a8f71f4e2758d08c28c564e8dfcce9f77776ad1"
      oracle =
        makeOracle
          $ Parameters
            {
              controlParameter = AssetClass (CurrencySymbol currency , TokenName "tBRIO")
            , datumParameter   = AssetClass (CurrencySymbol currency , TokenName "tSOFR")
            , feeToken         = AssetClass (CurrencySymbol currency , TokenName "tPIGY")
            , feeAmount        = 10
            }
    address <- exportOracle "oracle.plutus" oracle
    print address

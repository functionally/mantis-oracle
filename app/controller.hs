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
-- | Example data feed for the oracle.
--
-----------------------------------------------------------------------------


{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}


module Main (
-- * Entry point
  main
) where


import Control.Monad                (guard)
import Control.Monad.IO.Class       (liftIO)
import Data.Aeson                   ((.:), withObject)
import Data.Aeson.Types             (Parser, parseMaybe)
import Mantis.Oracle.Controller.PAB (runOraclePAB)
import Network.HTTP.Req             (GET(..), NoReqBody(..), (/:), defaultHttpConfig, https, jsonResponse, req, responseBody, runReq)
import PlutusTx                     (Data(..))

import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.Vector           as V  ((!), null)


-- | Poll for new oracle data and send it to the oracle.
main :: IO () -- ^ Action for controlling the oracle.
main =
  runOraclePAB
    3600_000_000
    "127.0.0.1"
    8080
    "oracle.cid"
    getSofr


-- | Fetch the latest SOFR percent rate from the NY Federal Reserve API.
getSofr :: IO (Maybe Data) -- ^ Action for fetching the latest SOFR.
getSofr =
  runReq defaultHttpConfig
    $ do
      result <-
        req
          GET
          (
            https "markets.newyorkfed.org"
              /: "api"
              /: "rates"
              /: "secured"
              /: "sofr"
              /: "last"
              /: "1.json"
          )
          NoReqBody
          jsonResponse
          mempty
      let
        sofr =
          flip parseMaybe (responseBody result)
            $ \o ->
              do
                refRates <- o .: "refRates"
                guard
                  . not $ V.null refRates
                flip (withObject "refRate") (refRates V.! 0) $ \o' ->
                  do
                    effectiveDate <- o' .: "effectiveDate"
                    percentRate   <- o' .: "percentRate" :: Parser Double
                    return
                      $ Map
                      [
                        (B "effectiveDate", B $ BS.pack effectiveDate     )
                      , (B "percentRate"  , B . BS.pack $ show percentRate)
                      ]
      liftIO . putStrLn $ "Got SOFR: " ++ show sofr
      return sofr

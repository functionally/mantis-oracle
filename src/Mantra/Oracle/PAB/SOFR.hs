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
-- | Oracle for NY Federal Reserver SOFR spot data.
--
-----------------------------------------------------------------------------


{-# LANGUAGE OverloadedStrings  #-}


module Mantra.Oracle.PAB.SOFR (
-- * Data sources
  fetchSOFR
) where


import Control.Monad          (guard)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson             ((.:), withObject)
import Data.Aeson.Types       (Parser, parseMaybe)
import Network.HTTP.Req       (GET(..), NoReqBody(..), (/:), defaultHttpConfig, https, jsonResponse, req, responseBody, runReq)
import PlutusTx               (Data(..))

import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.Vector           as V  ((!), null)


-- | Fetch the latest SOFR percent rate from the NY Federal Reserve API.
fetchSOFR :: IO (Maybe Data) -- ^ Action for fetching the latest SOFR.
fetchSOFR =
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

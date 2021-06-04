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
  main
) where


import Control.Concurrent       (threadDelay)
import Control.Monad            (guard, when)
import Control.Monad.IO.Class   (liftIO)
import Data.Aeson               ((.:), withObject)
import Data.Aeson.Types         (Parser, parseMaybe)
import Data.Proxy               (Proxy (..))
import Data.UUID                (UUID)
import Network.HTTP.Req         (GET(..), JsonResponse, NoReqBody(..), ReqBodyJson(..), POST(..), (/:), defaultHttpConfig, http, https, jsonResponse, port, req, responseBody, responseStatusCode, runReq)
import Plutus.V1.Ledger.Scripts ()
import PlutusTx                 (Data(..))

import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.Text             as T  (pack)
import qualified Data.Vector           as V  ((!), null)


main :: IO ()
main = runOracle getSofr


runOracle :: IO (Maybe Data)
          -> IO ()
runOracle getter =
  do
    uuid <- read <$> readFile "oracle.cid"
    putStrLn $ "Oracle contract instance id: " ++ show uuid
    let
      go m =
        do
          x <- getter
          when (m /= x)
            $ maybe (return ()) (updateOracle uuid) x
          threadDelay 3600_000_000
          go x
    go Nothing


updateOracle :: UUID
             -> Data
             -> IO ()
updateOracle uuid x =
  runReq defaultHttpConfig
    $ do
      v <-
        req
          POST
          (
            http "127.0.0.1"
              /: "api"
              /: "new"
              /: "contract"
              /: "instance"
              /: T.pack (show uuid)
              /: "endpoint"
              /: "write"
          )
          (ReqBodyJson x)
          (Proxy :: Proxy (JsonResponse ()))
          (port 8080)
      liftIO
        . putStrLn
        $ if responseStatusCode v == 200
          then "Updated oracle: " ++ show x ++ "."
          else "Error updating oracle."


getSofr :: IO (Maybe Data)
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
                flip (withObject "z") (refRates V.! 0) $ \o' ->
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

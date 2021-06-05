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
-- | Poll data and post it to the oracle.
--
-----------------------------------------------------------------------------


{-# LANGUAGE OverloadedStrings  #-}


module Mantis.Oracle.Controller.PAB (
-- * Entry point
  runOraclePAB
) where


import Control.Concurrent       (threadDelay)
import Control.Monad            (when)
import Control.Monad.IO.Class   (liftIO)
import Data.Proxy               (Proxy (..))
import Data.Text                (Text)
import Data.UUID                (UUID)
import Network.HTTP.Req         (JsonResponse, ReqBodyJson(..), POST(..), (/:), defaultHttpConfig, http, port, req, responseStatusCode, runReq)
import Plutus.V1.Ledger.Scripts ()
import PlutusTx                 (Data(..))

import qualified Data.Text as T (pack)


-- | Poll for new oracle data and send it to the oracle.
runOraclePAB :: Int             -- ^ Number of microseconds between polls for new data.
             -> Text            -- ^ The IP address of the PAB.
             -> Int             -- ^ The port number for the PAB.
             -> FilePath        -- ^ Path to the Contract ID (CID) file for the oracle instance.
             -> IO (Maybe Data) -- ^ Action for fetching data for the oracle.
             -> IO ()           -- ^ Action for controllng the oracle.
runOraclePAB delay address portNumber filename fetcher =
  do
    uuid <- read <$> readFile filename
    putStrLn $ "Oracle contract instance id: " ++ show uuid
    let
      go m =
        do
          x <- fetcher
          when (m /= x)
            $ maybe (return ()) (updateOracle address portNumber uuid) x
          threadDelay delay
          go x
    go Nothing


-- | Update the value of the oracle.
updateOracle :: Text  -- ^ The IP address of the PAB.
             -> Int   -- ^ The port number for the PAB.
             -> UUID  -- ^ The oracle's instance ID.
             -> Data  -- ^ The new data.
             -> IO () -- ^ Action fr updating the oracle.
updateOracle address portNumber uuid x =
  runReq defaultHttpConfig
    $ do
      v <-
        req
          POST
          (
            http address
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
          (port portNumber)
      liftIO
        . putStrLn
        $ if responseStatusCode v == 200
          then "Updated oracle: " ++ show x ++ "."
          else "Error updating oracle."

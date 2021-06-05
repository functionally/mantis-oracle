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
-- | Example client for the oracle.
--
-----------------------------------------------------------------------------


{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main (
-- * Entry point
  main
) where


import Control.Concurrent     (threadDelay)
import Control.Exception      (handle)
import Control.Monad          (void)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy             (Proxy (..))
import Data.Text              (Text)
import Data.UUID              (UUID)
import Network.HTTP.Req       (HttpException, JsonResponse, POST(..), ReqBodyJson(..), (/:), defaultHttpConfig, http, port, req, responseStatusCode, runReq)
import System.Environment     (getArgs)
import System.IO              (BufferMode(NoBuffering), hSetBuffering, stdout)

import qualified Data.Text as T (pack)


-- | Read the oracle data on demand.
main :: IO () -- ^ Action for reading the oracle data.
main =
  do
    [filename] <- getArgs
    uuid <- read <$> readFile filename
    let
      go =
        do
          putStrLn "Press enter to read oracle."
          void getLine
          readOracle "127.0.0.1" 8080 uuid
          go
    hSetBuffering stdout NoBuffering
    putStrLn $ "Client instance id for Wallet " ++ show uuid ++ "."
    go


-- | Read the oracle data.
readOracle :: Text  -- ^ The IP address of the PAB.
           -> Int   -- ^ The port number of the PAB.
           -> UUID  -- ^ The Contract ID (CID) for the oracle.
           -> IO () -- ^ Action for reading the oracle.
readOracle address portNumber uuid =
  let
    h :: HttpException -> IO ()
    h _ = threadDelay 1_000_000 >> readOracle address portNumber uuid
  in
    handle h
      . runReq defaultHttpConfig
      $ do
        result <-
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
                 /: "read"
            )
            (ReqBodyJson ())
            (Proxy :: Proxy (JsonResponse ()))
            (port portNumber)
        liftIO
          $ if responseStatusCode result /= 200
            then putStrLn "Error reading oracle."
            else putStrLn "Success reading oracle."

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
  main
) where


import Control.Concurrent     (threadDelay)
import Control.Exception      (handle)
import Control.Monad          (void)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy             (Proxy (..))
import Data.UUID              (UUID)
import Network.HTTP.Req       (HttpException, JsonResponse, POST(..), ReqBodyJson(..), (/:), defaultHttpConfig, http, port, req, responseStatusCode, runReq)
import System.Environment     (getArgs)
import System.IO              (BufferMode(NoBuffering), hSetBuffering, stdout)

import qualified Data.Text as T (pack)


main :: IO ()
main =
  do
    [i :: Int] <- map read <$> getArgs
    uuid <- read <$> readFile ('W' : show i ++ ".cid")
    let
      go =
        do
          putStrLn "Press enter to read oracle."
          void getLine
          readOracle uuid
          go
    hSetBuffering stdout NoBuffering
    putStrLn $ "Client instance id for Wallet " ++ show i ++ ": " ++ show uuid
    go


readOracle :: UUID
           -> IO ()
readOracle uuid =
  let
    h :: HttpException -> IO ()
    h _ = threadDelay 1_000_000 >> readOracle uuid
  in
    handle h
      . runReq defaultHttpConfig
      $ do
        result <-
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
                 /: "read"
            )
            (ReqBodyJson ())
            (Proxy :: Proxy (JsonResponse ()))
            (port 8080)
        liftIO
          $ if responseStatusCode result /= 200
            then putStrLn "Error reading oracle."
            else putStrLn "Success reading oracle."

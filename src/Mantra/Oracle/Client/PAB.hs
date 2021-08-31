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
-- | Bare-bones PAB client for the oracle.
--
-----------------------------------------------------------------------------


{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Mantra.Oracle.Client.PAB (
-- * Entry point
  readOraclePAB
) where


import Control.Concurrent     (threadDelay)
import Control.Exception      (handle)
import Control.Monad.IO.Class (liftIO)
import Data.Proxy             (Proxy (..))
import Data.Text              (Text)
import Data.UUID              (UUID)
import Network.HTTP.Req       (HttpException, JsonResponse, POST(..), ReqBodyJson(..), (/:), defaultHttpConfig, http, port, req, responseStatusCode, runReq)

import qualified Data.Text as T (pack)


-- | Read the oracle data in a contract on the PAB.
readOraclePAB :: Text  -- ^ The IP address of the PAB.
              -> Int   -- ^ The port number of the PAB.
              -> UUID  -- ^ The Contract ID (CID) for the oracle.
              -> IO () -- ^ Action for reading the oracle.
readOraclePAB address portNumber uuid =
  let
    h :: HttpException -> IO ()
    h _ = threadDelay 1_000_000 >> readOraclePAB address portNumber uuid
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
            then putStrLn "Error submitting transaction to read oracle."
            else putStrLn "Success submitting transaction to read oracle."

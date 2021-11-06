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
-- | Submit transactions periodically.
--
-----------------------------------------------------------------------------


module Mantra.Oracle.Loop (
-- * Operations
  loopOracle
) where


import Cardano.Api               (AddressAny, CardanoMode, Lovelace, LocalNodeConnectInfo, NetworkId, PaymentKey, Quantity, SigningKey, TxId)
import Control.Monad.Except      (throwError, liftIO)
import Control.Time              (delayTill)
import Data.Time.Clock           (addUTCTime, getCurrentTime)
import Data.Word                 (Word64)
import Development.Shake.Command (Exit(..), Stderr(..), Stdout(..), cmd)
import Mantra.Oracle.Submit      (writeOracle)
import Mantra.Oracle.Types       (Oracle(..))
import Mantra.Types              (MantraM, foistMantraMaybeIO)
import System.Exit               (ExitCode(..))

import qualified Data.Aeson as A (decodeFileStrict, Value(..))


-- | Submit the transaction to write new data to an oracle. The payment address must have an eUTxO containing the control token; it also must have at least one other UTxO containing no tokens.
loopOracle :: A.Value                          -- ^ The datum currently held in the script.
           -> FilePath                         -- ^ The path to the script to update the datum.
           -> Int                              -- ^ The delay in seconds for first updating the datum.
           -> Int                              -- ^ The frequency in seconds for updating the datum.
           -> Maybe Word64                     -- ^ The metadata key for the new datum, if any.
           -> Maybe A.Value                    -- ^ The metadata message, if any.
           -> LocalNodeConnectInfo CardanoMode -- ^ The connection info for the local node.
           -> NetworkId                        -- ^ The network identifier.
           -> Oracle                           -- ^ The oracle.
           -> AddressAny                       -- ^ The payment address.
           -> SigningKey PaymentKey            -- ^ The signing key for the payment address.
           -> Lovelace                         -- ^ The maximum value for collateral.
           -> Quantity                         -- ^ The Lovelace to be sent to the script.
           -> MantraM IO TxId                  -- ^ Action to submit the writing transaction.
loopOracle oldData updateScript updateDelay updateFrequency metadataKey message connection network oracle controlAddress controlSigning maxCollateral scriptLovelace =
  do
    let
      updateDelay'     = toEnum $ 10^(12::Int) * updateDelay
      updateFrequency' = toEnum $ 10^(12::Int) * updateFrequency
      go oldData' time =
        do
          liftIO $ putStrLn ""
          liftIO . putStrLn $ "Timestamp: " ++ show time
          (Exit code, Stdout result, Stderr msg) <- liftIO $ cmd updateScript
          case (code, lines result) of
            (ExitSuccess  , [newDataFile]) -> do
                                                newData <-
                                                  foistMantraMaybeIO "Failed reading new data JSON."
                                                    $ A.decodeFileStrict newDataFile
                                                liftIO . putStrLn $ "New datum file: " ++ show newDataFile
                                                txId <-
                                                  writeOracle oldData' newData
                                                    metadataKey message connection network oracle controlAddress controlSigning maxCollateral scriptLovelace
                                                liftIO . putStrLn $ "TxId " ++ show txId
                                                let
                                                  time' = updateFrequency' `addUTCTime` time
                                                delayTill time'
                                                go newData time'
            (ExitSuccess  , _            ) -> throwError "Update script did not return a single filepath to new datum."
            (ExitFailure _, _            ) -> throwError msg
    start <- addUTCTime updateDelay' <$> liftIO getCurrentTime
    delayTill start
    go oldData start

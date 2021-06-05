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


import Data.Text                    (Text)
import Data.Version                 (showVersion)
import Ledger.Value                 (CurrencySymbol(..), TokenName(..))
import Mantis.Oracle.Client.PAB     (readOraclePAB)
import Mantis.Oracle.Controller.PAB (runOraclePAB)
import Mantis.Oracle.SOFR           (fetchSOFR)
import Paths_mantis_oracle          (version)
import Wallet.Emulator.Wallet       (Wallet(..))

import qualified Data.ByteString.Char8      as BS       (pack)
import qualified Mantis.Oracle.Simulate     as Simulate (main)
import qualified Mantis.Oracle.Simulate.PAB as Simulate (runPAB)
import qualified Options.Applicative        as O


-- | Available commands.
data Command =
    Test
    {
      currency    :: String
    , controlName :: String
    , datumName   :: String
    , feeName     :: String
    , feeAmount   :: Integer
    }
  | Simulate
    {
      controlName :: String
    , datumName   :: String
    , feeName     :: String
    , feeAmount   :: Integer
    , wallets     :: [(Integer, Integer, FilePath)]
    }
  | Control
    {
      frequency   :: Int
    , host        :: Text
    , port        :: Int
    , oracle      :: FilePath
    }
  | Employ
    {
      host   :: Text
    , port   :: Int
    , wallet :: FilePath
    }
    deriving (Eq, Ord, Read, Show)


-- | Run the example.
main :: IO () -- ^ Action for running the example.
main =
  do
    let
      versionOption =
        O.infoOption
          ("Mantis Oracle " ++ showVersion version ++ ", (c) 2021 Brian W Bush <code@functionally.io>")
          (O.long "version" <> O.help "Show version.")
      parser =
        O.info
        (
               O.helper
           <*> versionOption
           <*> O.hsubparser (
                    O.command "trace"
                    (
                      O.info
                        (
                          Test
                            <$> O.strArgument     (O.metavar "CURRENCY"     <> O.help "Currency symbol for the tokens."            )
                            <*> O.strArgument     (O.metavar "CONTROL_NAME" <> O.help "Name of control token."                     )
                            <*> O.strArgument     (O.metavar "DATUM_NAME"   <> O.help "Name of datum token."                       )
                            <*> O.strArgument     (O.metavar "FEE_NAME"     <> O.help "Name of fee token."                         )
                            <*> O.argument O.auto (O.metavar "FEE_AMOUNT"   <> O.help "Number of fee tokens needed to read oracle.")
                        )
                        $ O.progDesc "Run an example oracle in a simulation trace."
                    )
                 <> O.command "simulate"
                    (
                      O.info
                        (
                          Simulate
                            <$> O.strArgument     (O.metavar "CONTROL_NAME" <> O.help "Name of control token."                                                                                                                                          )
                            <*> O.strArgument     (O.metavar "DATUM_NAME"   <> O.help "Name of datum token."                                                                                                                                            )
                            <*> O.strArgument     (O.metavar "FEE_NAME"     <> O.help "Name of fee token."                                                                                                                                              )
                            <*> O.argument O.auto (O.metavar "FEE_AMOUNT"   <> O.help "Number of fee tokens needed to read oracle."                                                                                                                     )
                            <*> O.argument O.auto (O.metavar "WALLETS"      <> O.help "Wallet information in for '[(i, a, cid)]', where <i> is the wallet number, <a> is the number of fee tokens in the wallet, and <cid> is the path to the CID file.")
                        )
                        $ O.progDesc "Run an example oracle in the PAB simulator."
                    )
                 <> O.command "control"
                    (
                      O.info
                        (
                          Control
                            <$> O.argument O.auto (O.metavar "POLL_DELAY" <> O.help "Number of seconds between polls of data source.")
                            <*> O.strArgument     (O.metavar "HOST"       <> O.help "The PAB host name."                             )
                            <*> O.argument O.auto (O.metavar "PORT"       <> O.help "The PAB port number."                           )
                            <*> O.strArgument     (O.metavar "ORACLE_CID" <> O.help "The Contract ID (CID) for the oracle instance." )
                        )
                        $ O.progDesc "Run an oracle in the PAB."
                    )
                 <> O.command "employ"
                    (
                      O.info
                        (
                          Employ
                            <$> O.strArgument     (O.metavar "HOST"       <> O.help "The PAB host name."                            )
                            <*> O.argument O.auto (O.metavar "PORT"       <> O.help "The PAB port number."                          )
                            <*> O.strArgument     (O.metavar "WALLET_CID" <> O.help "The Contract ID (CID) for the wallet instance.")
                        )
                        $ O.progDesc "Employ an oracle in the PAB."
                    )
               )
        )
        (
             O.fullDesc
          <> O.progDesc "Utilities for a Cardano oracle."
          <> O.header "Mantis oracle tool."
        )
    command <- O.execParser parser
    case command of
      Test{..}     -> Simulate.main
                        (CurrencySymbol $ BS.pack currency   )
                        (TokenName      $ BS.pack controlName)
                        (TokenName      $ BS.pack datumName  )
                        (TokenName      $ BS.pack feeName    )
                        feeAmount
      Simulate{..} -> Simulate.runPAB
                        (TokenName      $ BS.pack controlName)
                        (TokenName      $ BS.pack datumName  )
                        (TokenName      $ BS.pack feeName    )
                        feeAmount
                        [(Wallet i, a, f) | (i, a, f) <- wallets]
      Control{..}  -> runOraclePAB
                        (frequency * 1_000_000)
                        host
                        port
                        oracle
                        fetchSOFR
      Employ{..}   -> do
                        uuid <- read <$> readFile wallet
                        readOraclePAB
                          host
                          port
                          uuid
                        

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


{-# LANGUAGE CPP                #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}


module Main (
-- * Commands
  main
) where


import Cardano.Api          -- (ConsensusModeParams(..), EpochSlots(..), LocalNodeConnectInfo(..), NetworkId(..), NetworkMagic(..), serialiseAddress)
import Control.Monad.Except (liftIO)
import Data.String          (fromString)
import Data.Version         (showVersion)
import Data.Word            (Word32, Word64)
import Ledger.Value         (AssetClass, CurrencySymbol(..), TokenName(..), assetClass)
import Mantis.Oracle        (exportOracle)
import Mantis.Oracle.Submit (createOracle, deleteOracle, writeOracle)
import Mantis.Oracle.Types  (Parameters(..), makeOracle)
import Mantis.Types
import Paths_mantis_oracle  (version)

import qualified Data.Aeson             as A
import qualified Data.ByteString.Base16 as Base16 (decode)
import qualified Data.ByteString.Char8  as BS     (pack)
import qualified Data.Text              as T      (pack, unpack)
import qualified Options.Applicative    as O
import qualified PlutusTx.Prelude       as P      (toBuiltin)

#if USE_PAB

import Data.Text                    (Text)
import Mantis.Oracle.Client.PAB     (readOraclePAB)
import Mantis.Oracle.Controller.PAB (runOraclePAB)
import Mantis.Oracle.SOFR           (fetchSOFR)
import Wallet.Emulator.Wallet       (Wallet(..))

import qualified Mantis.Oracle.Simulate     as Simulate (main)
import qualified Mantis.Oracle.Simulate.PAB as Simulate (runPAB)

#endif


-- | Node and oracle configuration.
data Configuration =
  Configuration
  {
    socketPath     :: FilePath     -- ^ The Cardano node socket location.
  , magic          :: Maybe Word32 -- ^ The network magic, or `Nothing` for mainnet.
  , epochSlots     :: Word64       -- ^ The number of slots per epoch.
  , controlAsset   :: String       -- ^ The control token policiy ID followed by a period and the asset name.
  , datumAsset     :: String       -- ^ The datum token policiy ID followed by a period and the asset name.
  , feeAsset       :: String       -- ^ The fee token policiy ID followed by a period and the asset name.
  , feeAmount      :: Integer      -- ^ The number of fee tokens required to read the oracle.
  , lovelaceAmount :: Integer      -- ^ The number of Lovelace required to read the oracle.
  }
    deriving (Read, Show)


-- | Available commands.
data Command =
    Export
    {
      configFile :: FilePath
    , output     :: FilePath
    }
  | Create
    {
      configFile     :: FilePath
    , signingAddress :: String
    , signingKeyFile :: FilePath
    , newDataFile    :: FilePath
    , metadataKey    :: Maybe Word64
    , messageFile    :: Maybe FilePath
    , minLovelace    :: Maybe Integer
    }
  | Delete
    {
      configFile     :: FilePath
    , signingAddress :: String
    , signingKeyFile :: FilePath
    , oldDataFile    :: FilePath
    , messageFile    :: Maybe FilePath
    , minLovelace    :: Maybe Integer
    }
  | Write
    {
      configFile     :: FilePath
    , signingAddress :: String
    , signingKeyFile :: FilePath
    , oldDataFile    :: FilePath
    , newDataFile    :: FilePath
    , metadataKey    :: Maybe Word64
    , messageFile    :: Maybe FilePath
    , minLovelace    :: Maybe Integer
    }
#if USE_PAB
  | Test
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
#endif
    deriving (Eq, Ord, Read, Show)


-- | Run an oracle command.
main :: IO () -- ^ Action for running the command.
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
                    O.command "export"
                    (
                      O.info
                        (
                          Export
                            <$> O.strArgument (O.metavar "CONFIG_FILE" <> O.help "The configuration file."                      )
                            <*> O.strArgument (O.metavar "OUTPUT_FILE" <> O.help "Output filename for the serialized validator.")
                        )
                        $ O.progDesc "Export the validator code and compute its address."
                    )
                 <> O.command "create"
                    (
                      O.info
                        (
                          Create
                            <$> O.strArgument               (                      O.metavar "CONFIG_FILE"     <> O.help "The configuration file."                    )
                            <*> O.strArgument               (                      O.metavar "SIGNING_ADDRESS" <> O.help "The address for the signing key."           )
                            <*> O.strArgument               (                      O.metavar "SIGNING_FILE"    <> O.help "The signing key file."                      )
                            <*> O.strArgument               (                      O.metavar "NEW_JSON_FILE"   <> O.help "The JSON file for the new oracle data."     )
                            <*> O.optional (O.option O.auto $ O.long "metadata" <> O.metavar "INTEGER"         <> O.help "The metadata key for the oracle data."      )
                            <*> O.optional (O.strOption     $ O.long "message"  <> O.metavar "JSON_FILE"       <> O.help "The JSON file for the message metadata."    )
                            <*> O.optional (O.option O.auto $ O.long "lovelace" <> O.metavar "INTEGER"         <> O.help "The minimum Lovelace for outputs."          )
                        )
                        $ O.progDesc "Write a value to the oracle."
                    )
                 <> O.command "delete"
                    (
                      O.info
                        (
                          Delete
                            <$> O.strArgument               (                      O.metavar "CONFIG_FILE"     <> O.help "The configuration file."                    )
                            <*> O.strArgument               (                      O.metavar "SIGNING_ADDRESS" <> O.help "The address for the signing key."           )
                            <*> O.strArgument               (                      O.metavar "SIGNING_FILE"    <> O.help "The signing key file."                      )
                            <*> O.strArgument               (                      O.metavar "OLD_JSON_FILE"   <> O.help "The JSON file for the existing oracle data.")
                            <*> O.optional (O.strOption     $ O.long "message"  <> O.metavar "JSON_FILE"       <> O.help "The JSON file for the message metadata."    )
                            <*> O.optional (O.option O.auto $ O.long "lovelace" <> O.metavar "INTEGER"         <> O.help "The minimum Lovelace for outputs."          )
                        )
                        $ O.progDesc "Write a value to the oracle."
                    )
                 <> O.command "write"
                    (
                      O.info
                        (
                          Write
                            <$> O.strArgument               (                      O.metavar "CONFIG_FILE"     <> O.help "The configuration file."                    )
                            <*> O.strArgument               (                      O.metavar "SIGNING_ADDRESS" <> O.help "The address for the signing key."           )
                            <*> O.strArgument               (                      O.metavar "SIGNING_FILE"    <> O.help "The signing key file."                      )
                            <*> O.strArgument               (                      O.metavar "OLD_JSON_FILE"   <> O.help "The JSON file for the existing oracle data.")
                            <*> O.strArgument               (                      O.metavar "NEW_JSON_FILE"   <> O.help "The JSON file for the new oracle data."     )
                            <*> O.optional (O.option O.auto $ O.long "metadata" <> O.metavar "INTEGER"         <> O.help "The metadata key for the oracle data."      )
                            <*> O.optional (O.strOption     $ O.long "message"  <> O.metavar "JSON_FILE"       <> O.help "The JSON file for the message metadata."    )
                            <*> O.optional (O.option O.auto $ O.long "lovelace" <> O.metavar "INTEGER"         <> O.help "The minimum Lovelace for outputs."          )
                        )
                        $ O.progDesc "Write a value to the oracle."
                    )
#if USE_PAB
                 <> O.command "trace"
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
#endif
               )
        )
        (
             O.fullDesc
          <> O.progDesc "Utilities for a Cardano oracle."
          <> O.header "Mantis oracle tool."
        )
    command <- O.execParser parser
    let
      -- Run a command to `IO`.
      run action =
        do
          result <- runMantisToIO action
          case result of
            Right txId    -> putStrLn $ "TxId " ++ show txId
            Left message' -> putStrLn message'
      -- Execute an operation.
      operate op =
        do
          -- Read the configuration.
          Configuration{..} <- liftIO $ read <$> readFile (configFile command)
          let
            -- Prepare for connecting to the node.
            network = maybe Mainnet (Testnet . NetworkMagic) magic
            connection =
              LocalNodeConnectInfo
              {
                localConsensusModeParams = CardanoModeParams $ EpochSlots epochSlots
              , localNodeNetworkId       = network
              , localNodeSocketPath      = socketPath
              }
            -- Construct the oracle.
            oracle =
              makeOracle
                $ Parameters
                  (readAssetClass controlAsset)
                  (readAssetClass datumAsset  )
                  (readAssetClass feeAsset    )
                  feeAmount
                  lovelaceAmount
          -- Read the signing address.
          signingAddress' <-
            foistMantisMaybe "Failed to read signing address"
              . deserialiseAddress AsAddressAny
              . T.pack
              $ signingAddress command
          -- Read the signing key.
          signingKey <-
            foistMantisEitherIO
              . readFileTextEnvelope (AsSigningKey AsPaymentKey)
              $ signingKeyFile command
          -- Read the metadata message, if any.
          message <-
            maybe
              (return Nothing)
              (foistMantisMaybeIO "Failed reading message." . A.decodeFileStrict)
              $ messageFile command
          -- Operate the oracle.
          op
            message
            connection
            network
            oracle
            signingAddress'
            signingKey
            (maybe 5_000_000 Quantity $ minLovelace command)
    case command of
      Export{..}   -> do
                        Configuration{..} <- read <$> readFile configFile
                        let
                          network = maybe Mainnet (Testnet . NetworkMagic) magic
                        address <-
                          exportOracle output network
                            . makeOracle
                            $ Parameters
                              (readAssetClass controlAsset)
                              (readAssetClass datumAsset  )
                              (readAssetClass feeAsset    )
                              feeAmount
                              lovelaceAmount
                        putStrLn . T.unpack . serialiseAddress $ address
      Create{..}   -> run
                        $ do
                          newData <-
                            foistMantisMaybeIO "Failed reading new data."
                              . A.decodeFileStrict
                              $ newDataFile
                          operate
                            $ createOracle
                                newData
                                metadataKey
      Delete{..}   -> run
                        $ do
                          oldData <-
                            foistMantisMaybeIO "Failed reading old data."
                              . A.decodeFileStrict
                              $ oldDataFile
                          operate
                            $ deleteOracle
                                oldData
      Write{..}    -> run
                        $ do
                          oldData <-
                            foistMantisMaybeIO "Failed reading old data."
                              . A.decodeFileStrict
                              $ oldDataFile
                          newData <-
                            foistMantisMaybeIO "Failed reading new data."
                              . A.decodeFileStrict
                              $ newDataFile
                          operate
                            $ writeOracle
                                oldData
                                newData
                                metadataKey
#if USE_PAB
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
#endif


-- | Read an asset class from a string with the policy ID in hexadecimal followed by a period and the asset name.
readAssetClass :: String     -- ^ The policy ID followed by a period and the asset name.
               -> AssetClass -- ^ The asset class.
readAssetClass text =
  let
    Right currency = fmap P.toBuiltin . Base16.decode . BS.pack $ takeWhile (/= '.') text
    name           = fromString . tail $ dropWhile (/= '.') text
  in
    assetClass 
      (CurrencySymbol currency)
      (TokenName      name    )

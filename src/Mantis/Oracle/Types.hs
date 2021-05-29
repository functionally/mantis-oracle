{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}


module Mantis.Oracle.Types (
  Oracle(..)
, makeOracle
, Parameters(..)
, Action(..)
) where


import PlutusTx.Prelude

import Data.Aeson   (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger.Value (AssetClass(..), Value)

import qualified Ledger.Value as Value   (singleton)
import qualified Prelude      as Haskell (Eq)


data Oracle =
  Oracle
  {
    controlToken :: !AssetClass
  , datumToken   :: !AssetClass
  , requiredFee  :: !Value
  }
    deriving (Haskell.Eq, Generic, FromJSON, Show, ToJSON)


data Parameters =
  Parameters
  {
    controlParameter :: AssetClass
  , datumParameter   :: AssetClass
  , feeToken         :: AssetClass
  , feeAmount        :: Integer
  }
    deriving (Haskell.Eq, Generic, FromJSON, Show, ToJSON)


makeOracle :: Parameters
           -> Oracle
makeOracle Parameters{..} =
  let
    controlToken = controlParameter
    datumToken   = datumParameter
    (symbol, name) = unAssetClass feeToken
    requiredFee = Value.singleton symbol name feeAmount
  in
    Oracle{..}


data Action =
    Delete
  | Read
  | Write
    deriving Show

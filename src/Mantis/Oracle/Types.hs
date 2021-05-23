{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Mantis.Oracle.Types (
  Oracle(..)
, Action(..)
, Parameters(..)
) where


import PlutusTx.Prelude

import Data.Aeson   (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Ledger       (PubKeyHash)
import Ledger.Value (AssetClass)

import qualified Prelude as Haskell (Eq, Ord)


data Oracle =
  Oracle
  {
    owner :: !PubKeyHash
  , token :: !AssetClass
  , fee   :: !Integer
  }
    deriving (Haskell.Eq, Generic, FromJSON, Haskell.Ord, Show, ToJSON)


data Action =
    Delete
  | Read
  | Write
    deriving Show


data Parameters =
  Parameters
  {
    oracleToken :: !AssetClass
  , oracleFee   :: !Integer
  }
    deriving (Generic, FromJSON, Show, ToJSON)

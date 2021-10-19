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
-- | Types describing the general-purpose datum oracle.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE RecordWildCards    #-}


module Mantra.Oracle.Types (
-- * Types
  Oracle(..)
, Parameters(..)
, Action(..)
-- * Constructors
, makeOracle
) where


import PlutusTx.Prelude

import Data.Aeson             (FromJSON, ToJSON)
import GHC.Generics           (Generic)
import Plutus.V1.Ledger.Ada   (lovelaceValueOf)
import Plutus.V1.Ledger.Value (AssetClass(..), Value)

import qualified Ledger.Value as Value   (singleton)
import qualified Prelude      as Haskell (Eq, Show)


-- | An oracle controlled by one token, holding another token, and requiring a fee for its use.
data Oracle =
  Oracle
  {
    controlToken :: !AssetClass -- ^ The token needed for writing or deleting the oracle.
  , datumToken   :: !AssetClass -- ^ The token with which the oracle datum is always associated.
  , requiredFee  :: !Value      -- ^ The minimum fee required to read the oracle on-chain.
  }
    deriving (Haskell.Eq, Generic, FromJSON, Haskell.Show, ToJSON)


-- | Parameters defining the oracle.
data Parameters =
  Parameters
  {
    controlParameter :: AssetClass -- ^ The token needed for writing or deleting the oracle.
  , datumParameter   :: AssetClass -- ^ The token with which the oracle datum is always associated.
  , feeToken         :: AssetClass -- ^ The token in which the fee must be paid for reading the oracle.
  , feeAmount        :: Integer    -- ^ The amount of the fee token needed for reading the oracle, if any.
  , lovelaceAmount   :: Integer    -- ^ The amount of Lovelace needed for reading the oracle, if any.
  }
    deriving (Haskell.Eq, Generic, FromJSON, Haskell.Show, ToJSON)


-- | Construct an oracle.
makeOracle :: Parameters -- ^ Parameters describing the oracle.
           -> Oracle     -- ^ The oracle.
makeOracle Parameters{..} =
  let
    controlToken = controlParameter
    datumToken   = datumParameter
    (symbol, name) = unAssetClass feeToken
    requiredFee =
      case (feeAmount, lovelaceAmount) of
        (0, 0) -> mempty
        (0, _) -> lovelaceValueOf lovelaceAmount
        (_, 0) -> Value.singleton symbol name feeAmount
        _      -> Value.singleton symbol name feeAmount <> lovelaceValueOf lovelaceAmount
  in
    Oracle{..}


-- | Redeemers for the oracle.
data Action =
    Delete -- ^ Delete (close) the oracle.
  | Read   -- ^ Read the oracle's datum.
  | Write  -- ^ Set or update the oracle's datum.
    deriving Haskell.Show

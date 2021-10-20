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
-- | Orphan instances.
--
-----------------------------------------------------------------------------


{-# OPTIONS_GHC -fno-warn-orphans #-}


module Mantra.Orphans (
) where


import PlutusTx (Data)
import Schema   (FormSchema(..), ToSchema(..))


instance ToSchema Data where
  toSchema = FormSchemaUnsupported "No schema is available for generic data." -- FIXME: Implement this!

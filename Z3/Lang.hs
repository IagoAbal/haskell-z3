
-- |
-- Module    : Z3.Lang
-- Copyright : (c) Iago Abal, 2012
--             (c) David Castro, 2012
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>,
--             David Castro <david.castro.dcp@gmail.com>
-- Stability : experimental

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
  -- Imports deprecated modules, but it is a depcrecated module itself.

module Z3.Lang
  {-# DEPRECATED
        "The Z3.Lang interface will be moved to a dedicated package."
        #-}
  ( module Z3.Lang.Prelude
  , module Z3.Lang.Nat
  ) where


import Z3.Lang.Prelude
import Z3.Lang.Nat

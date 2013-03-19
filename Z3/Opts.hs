
-- |
-- Module    : Z3.Opts
-- Copyright : (c) Iago Abal, 2013
--             (c) David Castro, 2013
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>,
--             David Castro <david.castro.dcp@gmail.com>
--
-- High-level interface to configuration options.

module Z3.Opts
  ( Opts
  , setOpts
  , stdOpts
  , (+?)
  , opt
  , OptValue
  )
  where

import qualified Z3.Base as Base

import Data.Monoid ( Monoid(..) )

---------------------------------------------------------------------
-- Configuration

-- | Z3 configuration.
newtype Opts = Opts [Opt]

instance Monoid Opts where
  mempty = Opts []
  mappend (Opts ps1) (Opts ps2) = Opts (ps1++ps2)

-- | Default configuration.
stdOpts :: Opts
stdOpts = mempty

-- | Append configurations.
(+?) :: Opts -> Opts -> Opts
(+?) = mappend

-- | Set a configuration option.
opt :: OptValue val => String -> val -> Opts
opt oid val = Opts [option oid val]

-- | Set configuration.
setOpts :: Base.Config -> Opts -> IO ()
setOpts baseCfg (Opts params) = mapM_ (setOpt baseCfg) params

-------------------------------------------------
-- Options

-- | Configuration option.
data Opt = Opt String  -- id
               String  -- value

-- | Set an option.
setOpt :: Base.Config -> Opt -> IO ()
setOpt baseCfg (Opt oid val) = Base.setParamValue baseCfg oid val

-- | Values for Z3 options.
class OptValue val where
  option :: String -> val -> Opt

instance OptValue Bool where
  option oid True  = Opt oid "true"
  option oid False = Opt oid "false"

instance OptValue Int where
  option oid = Opt oid . show

instance OptValue Integer where
  option oid = Opt oid . show

instance OptValue Double where
  option oid = Opt oid . show

instance OptValue [Char] where
  option = Opt


-- |
-- Module    : Z3.Opts
-- Copyright : (c) Iago Abal, 2013-2014
--             (c) David Castro, 2013
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>,
--             David Castro <david.castro.dcp@gmail.com>
--
-- Configuring Z3.
--
-- Z3 has plenty of configuration options and these vary quite a lot
-- across Z3 versions, being hard to design a proper abstraction.
-- We decided to keep this simple.

module Z3.Opts
  ( -- * Z3 configuration
    Opts
  , setOpts
  , stdOpts
  , (+?)
    -- * Z3 options
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

singleton :: Opt -> Opts
singleton o = Opts [o]

-- | Default configuration.
stdOpts :: Opts
stdOpts = mempty

-- | Append configurations.
(+?) :: Opts -> Opts -> Opts
(+?) = mappend

-- | Set a configuration option.
opt :: OptValue val => String -> val -> Opts
opt oid val = singleton $ option oid val

-- | Set configuration.
--
-- If you are using 'Z3.Lang' or 'Z3.Monad' interfaces, you don't need
-- to call this function directly, just pass your 'Opts' to /evalZ3*/.
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
  option oid = Opt oid . boolVal

instance OptValue Int where
  option oid = Opt oid . show

instance OptValue Integer where
  option oid = Opt oid . show

instance OptValue Double where
  option oid = Opt oid . show

instance OptValue [Char] where
  option = Opt

-------------------------------------------------
-- Utils

boolVal :: Bool -> String
boolVal True  = "true"
boolVal False = "false"

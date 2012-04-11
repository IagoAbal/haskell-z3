{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module    : Z3.Base
-- Copyright : (c) Iago Abal, 2011 
--             (c) David Castro, 2011
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>, 
--             David Castro <david.castro.dcp@gmail.com>

module Z3.Base (

    -- * Core Z3 types
      Config
    , Context
    , Symbol
    , AST
    , Sort
    , App
    , Pattern
    , Model
    
    -- ** Utility types
    , Result(..)

    ) where

import Z3.Base.C

import Foreign

------------------------------------------------------------------------
-- Types

-- | A Z3 /configuration object/.
-- 
--
-- /Notes:/
--
-- * The resource is automatically managed by the Haskell garbage
-- collector, and the structure is automatically deleted once it is out
-- of scope (no need to call 'z3_del_config').
--
-- /Reference:/ < TODO >
--
newtype Config = Config { _unConfig :: ForeignPtr Z3_config }
    deriving Eq

-- | A Z3 /logical context/.
-- 
--
-- /Notes:/
--
-- * The resource is automatically managed by the Haskell garbage
-- collector, and the structure is automatically deleted once it is out
-- of scope (no need to call 'z3_del_context').
--
-- /Reference:/ < TODO >
--
newtype Context = Context { _unContext :: ForeignPtr Z3_context }
    deriving Eq

-- | A Z3 /Lisp-link symbol/.
-- 
-- /Reference:/ < TODO >
--
newtype Symbol = Symbol { _unSymbol :: Ptr Z3_symbol }
    deriving (Eq, Ord, Show, Storable)

-- | A Z3 /AST node/.
-- 
-- /Reference:/ < TODO >
--
newtype AST = AST { _unAST :: Ptr Z3_ast }
    deriving (Eq, Ord, Show, Storable)

    -- TODO Improve type-safety with phantom types.

-- | Kind of Z3 AST representing /types/.
-- 
-- /Reference:/ < TODO >
--
newtype Sort = Sort { _unSort :: Ptr Z3_sort }
    deriving (Eq, Ord, Show, Storable)

-- | A kind of Z3 AST used to represent constant and function declarations.
-- 
-- /Reference:/ < TODO >
--
newtype App = App { _unApp :: Ptr Z3_app }
    deriving (Eq, Ord, Show, Storable)

-- | A kind of AST used to represent pattern and multi-patterns used to 
--   guide quantifier instantiation.
-- 
-- /Reference:/ < TODO >
--
newtype Pattern = Pattern { _unPattern :: Ptr Z3_pattern }
    deriving (Eq, Ord, Show, Storable)

-- | A model for the constraints asserted into the logical context.
-- 
-- /Reference:/ < TODO >
--
newtype Model = Model { _unModel :: Ptr Z3_model }
    deriving (Eq, Ord, Show, Storable)

-- | Lifted Boolean type.
--
-- /Reference:/ < TODO >
--
data Result
    = Satisfiable
    | Unsatisfiable
    | Undefined
    deriving (Eq, Ord, Enum, Bounded, Read, Show)


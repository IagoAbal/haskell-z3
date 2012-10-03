{-# OPTIONS_GHC -funbox-strict-fields #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeFamilies       #-}

-- |
-- Module    : Z3.Lang.Exprs
-- Copyright : (c) Iago Abal, 2012
--             (c) David Castro, 2012
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>,
--             David Castro <david.castro.dcp@gmail.com>
-- Stability : experimental
--

module Z3.Lang.Exprs (
    -- * Types
      TypeZ3
    , IsTy(..)
    , IsScalar(..)
    
    -- ** Numeric types
    , IsNum
    , IsInt
    , IsReal
    
    -- * Abstract syntax
    , Uniq
    , Expr (..)
    , BoolBinOp (..)
    , BoolMultiOp (..)
    , CRingOp (..)
    , IntOp (..)
    , RealOp (..)
    , CmpOpE (..)
    , CmpOpI (..)
    ) where

import {-# SOURCE #-} Z3.Lang.Monad ( Z3 )

import qualified Z3.Base as Base

import Data.Typeable ( Typeable )

----------------------------------------------------------------------
-- Types 

-- | Maps a type to the underlying Z3 type.
--
type family TypeZ3 a

-- | Types for expressions.
--
class (Typeable a, Base.Z3Type (TypeZ3 a)) => IsTy a where
  -- | Create a 'Base.AST' from a 'Expr'.
  --
  compile :: Expr a -> Z3 (Base.AST (TypeZ3 a))

-- | Scalar types.
--
class (Eq a, Show a, IsTy a, Base.Z3Scalar(TypeZ3 a)) => IsScalar a where
  fromZ3Type :: TypeZ3 a -> a
  toZ3Type   :: a -> TypeZ3 a

------------------------------------------------------------
-- Numeric types
--
-- Future Work: We would like to instance 'IsInt' with 'Int32' to provide
-- support for reasoning about 32-bit integer arithmetic with overflow.
-- It would be also interesting (but perhaps more tricky) to support
-- floating point arithmetic by creating an instance of 'IsReal' for
-- 'Double'.
--

-- | Numeric types.
--
class (IsScalar a, Num a, Base.Z3Num (TypeZ3 a)) => IsNum a where

-- | Typeclass for Haskell Z3 numbers of /int/ sort in Z3.
--
class (IsNum a, Integral a, TypeZ3 a ~ Integer) => IsInt a where

-- | Typeclass for Haskell Z3 numbers of /real/ sort in Z3.
--
class (IsNum a, Fractional a, Real a, TypeZ3 a ~ Rational) => IsReal a where

------------------------------------------------------------
-- Abstract syntax

-- | Unique identifiers
--
type Uniq = Int

-- | Abstract syntax.
--
data Expr :: * -> * where
  --  | Literals
  Lit :: IsScalar a => a -> Expr a
  --  | Constants
  Const :: !Uniq -> Expr a
  --  | Logical negation
  Not :: Expr Bool -> Expr Bool
  --  | Binary boolean expressions
  BoolBin :: BoolBinOp -> Expr Bool -> Expr Bool -> Expr Bool
  --  | Variadic boolean expressions
  BoolMulti :: BoolMultiOp -> [Expr Bool] -> Expr Bool
  --  | Arithmetic negation
  Neg :: IsNum a => Expr a -> Expr a
  --  | Arithmetic expressions for commutative rings
  CRingArith :: IsNum a => CRingOp -> [Expr a] -> Expr a
  --  | Integer arithmetic
  IntArith :: IsInt a => IntOp -> Expr a -> Expr a -> Expr a
  --  | Real arithmetic
  RealArith :: IsReal a => RealOp -> Expr a -> Expr a -> Expr a
  --  | Comparison expressions
  CmpE :: IsScalar a => CmpOpE -> Expr a -> Expr a -> Expr Bool
  CmpI :: IsNum a => CmpOpI -> Expr a -> Expr a -> Expr Bool
  --  | if-then-else expressions
  Ite :: IsTy a => Expr Bool -> Expr a -> Expr a -> Expr a

-- | Boolean binary operations.
data BoolBinOp = Xor | Implies | Iff
    deriving (Eq,Show)

-- | Boolean variadic operations.
data BoolMultiOp = And | Or
    deriving (Eq,Show)

-- | Commutative ring operations.
data CRingOp = Add | Mul | Sub
    deriving (Eq,Show)

-- | Operations for sort /int/.
data IntOp = Quot | Mod | Rem
    deriving (Eq,Show)

-- | Operations for sort /real/.
data RealOp = Div
    deriving (Eq,Show)

-- | Equality testing.
data CmpOpE = Eq | Neq
    deriving (Eq, Show, Typeable)

-- | Inequality comparisons.
data CmpOpI = Le | Lt | Ge | Gt
    deriving (Eq, Show, Typeable)

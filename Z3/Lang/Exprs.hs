{-# OPTIONS_GHC -funbox-strict-fields #-}

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

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
    , Compilable(..)
    , IsTy(..)
    , IsFun(..)
    
    -- ** Numeric types
    , IsNum
    , IsInt
    , IsReal
    
    -- * Abstract syntax
    , Uniq
    , Layout
    , Expr (..)
    , Pattern (..)
    , Quantifier(..)
    , QExpr(..)
    , Castable(..)
    , FunApp (..)
    , BoolBinOp (..)
    , BoolMultiOp (..)
    , CRingOp (..)
    , IntOp (..)
    , RealOp (..)
    , CmpOpE (..)
    , CmpOpI (..)

    -- * Type checking
    , typecheck
    , TCM
    , TCC
    , ok
    , withHypo
    , newTCC
    , evalTCM
    ) where

import {-# SOURCE #-} Z3.Lang.Monad ( Z3 )
import Z3.Lang.TY
import qualified Z3.Base as Base

import Control.Monad.RWS
import Data.Typeable ( Typeable )

----------------------------------------------------------------------
-- Types 

-- | Maps a type to the underlying Z3 type.
--
type family TypeZ3 a

type instance TypeZ3 (Expr a)   = TypeZ3 a
type instance TypeZ3 (FunApp a) = TypeZ3 a

-- | Compilable /things/.
--
class Compilable t where
  compile :: t -> Z3 Base.AST

-- | Types for expressions.
--
class (Eq a, Show a, Typeable a, Compilable (Expr a)) => IsTy a where
  -- | Type invariant.
  -- Introduced when creating a variable.
  --
  typeInv :: Expr a -> Expr Bool

  -- | Typecheck an expression.
  --
  tc :: Expr a -> TCM ()
  
  -- | Convert from underlying Z3 type to type.
  --
  fromZ3Type :: TypeZ3 a -> a
  
  -- | Convert from a type to its underlying Z3 type.
  --
  toZ3Type   :: a -> TypeZ3 a

  -- | Create a sort of the underlying Z3 type.
  --
  mkSort :: TY a -> Z3 Base.Sort

  -- | Create a value of the .
  --
  mkLiteral :: a -> Z3 Base.AST

  -- | Value extractor
  --
  getValue :: Base.AST -> Z3 a


-- | Function types.
--
class IsFun a where
  domain :: TY a -> Z3 [Base.Sort]
  range  :: TY a -> Z3 Base.Sort

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
class (IsTy a, Num a) => IsNum a where

-- | Typeclass for Haskell Z3 numbers of /int/ sort in Z3.
--
class (IsNum a, Integral a, TypeZ3 a ~ Integer) => IsInt a where

-- | Typeclass for Haskell Z3 numbers of /real/ sort in Z3.
--
class (IsNum a, Fractional a, Real a, TypeZ3 a ~ Rational) => IsReal a where

------------------------------------------------------------
-- Abstract syntax

-- | Unique identifiers.
--
type Uniq = Int

-- | Quantifier layout level.
--
type Layout = Int

-- | Abstract syntax.
--
data Expr :: * -> * where
  --  | Literals
  Lit :: IsTy a => a -> Expr a
  --  | Constants
  Const :: !Uniq -> Base.AST -> Expr a
  --  | Tag, for converting from HOAS to de-Bruijn
  Tag :: !Layout -> Expr a
  --  | Logical negation
  Not :: Expr Bool -> Expr Bool
  --  | Binary boolean expressions
  BoolBin :: BoolBinOp -> Expr Bool -> Expr Bool -> Expr Bool
  --  | Variadic boolean expressions
  BoolMulti :: BoolMultiOp -> [Expr Bool] -> Expr Bool
  --  | Quantified formula
  Quant :: QExpr t => Quantifier -> t -> Expr Bool
  --  | Arithmetic negation
  Neg :: IsNum a => Expr a -> Expr a
  --  | Arithmetic expressions for commutative rings
  CRingArith :: IsNum a => CRingOp -> [Expr a] -> Expr a
  --  | Integer arithmetic
  IntArith :: IsInt a => IntOp -> Expr a -> Expr a -> Expr a
  --  | Real arithmetic
  RealArith :: IsReal a => RealOp -> Expr a -> Expr a -> Expr a
  --  | Equality testing.
  CmpE :: IsTy a => CmpOpE -> [Expr a] -> Expr Bool
  --  | Ordering comparisons.
  CmpI :: IsNum a => CmpOpI -> Expr a -> Expr a -> Expr Bool
  --  | if-then-else expressions
  Ite :: IsTy a => Expr Bool -> Expr a -> Expr a -> Expr a
  --  | Application
  App :: IsTy a  => FunApp a -> Expr a
  --  | Casting between compatible types
  Cast :: (IsTy a, IsTy b, Castable a b) => Expr a -> Expr b

class QExpr t where
  compileQuant :: Quantifier -> [Base.Symbol] -> [Base.Sort] -> t -> Z3 Base.AST

class Castable a b where
  compileCast :: TY (a,b) -> Base.AST -> Z3 Base.AST


-- | Quantifier pattern.
--
data Pattern where
  Pat :: IsTy a => Expr a -> Pattern

-- | Quantifiers
data Quantifier = ForAll | Exists
  deriving (Eq, Show)

-- | Z3 function
--
data FunApp :: * -> * where
  --  | Function declaration
  FuncDecl :: IsFun a => Base.FuncDecl -> FunApp a
  --  | Partial application
  PApp :: IsTy a => FunApp (a -> b) -> Expr a -> FunApp b

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
data CmpOpE = Eq | Neq | Distinct
    deriving (Eq, Show, Typeable)

-- | Ordering comparisons.
data CmpOpI = Le | Lt | Ge | Gt
    deriving (Eq, Show, Typeable)

----------------------------------------------------------------------
-- Typecheck monad

newtype TCM a = TCM { unTCM :: RWS Context [TCC] () a }
    deriving Monad

type TCC = Expr Bool

type Context = [Expr Bool]

ok :: TCM ()
ok = return ()

withHypo :: Expr Bool -> TCM a -> TCM a
withHypo h = TCM . local (h:) . unTCM

newTCC :: [Expr Bool] -> TCM ()
newTCC tccs = TCM $ do
  hs <- ask
  tell $ map (mkTCC hs) tccs
  where mkTCC [] = id
        mkTCC hs = BoolBin Implies (BoolMulti And hs)

evalTCM :: TCM a -> (a,[TCC])
evalTCM m = evalRWS (unTCM m) [] ()

typecheck :: IsTy a => Expr a -> [Expr Bool]
typecheck e = snd $ evalTCM (tc e)

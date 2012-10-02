{-# OPTIONS_GHC -funbox-strict-fields #-}

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Module    : Z3.Lang.Monad
-- Copyright : (c) Iago Abal, 2012
--             (c) David Castro, 2012
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>,
--             David Castro <david.castro.dcp@gmail.com>
-- Stability : experimental

module Z3.Lang.Monad (
    -- * Types
      TypeZ3
    , IsTy(..)
    , IsScalar(..)
    
    -- ** Numeric types
    , IsNum
    , IsInt
    , IsReal
    
    -- * Abstract syntax
    , Expr (..)
    , BoolBinOp (..)
    , BoolMultiOp (..)
    , CRingOp (..)
    , IntOp (..)
    , RealOp (..)
    , CmpOpE (..)
    , CmpOpI (..)

    -- * Z3 Monad
    , Z3
    , Z3State(..)
    , evalZ3
    , fresh
    , addConst
    , getConst

    -- ** Lifted Z3.Base functions
    , assertCnstr
    , check
    , mkSort
    , mkStringSymbol
    , mkLiteral
    , mkNot
    , mkBoolBin
    , mkBoolMulti
    , mkEq
    , mkCmp
    , mkConst
    , mkUnaryMinus
    , mkCRingArith
    , mkIntArith
    , mkRealArith
    , mkIte
    
    -- * Satisfiability result
    , Base.Result(..)
    
    ) where

import qualified Z3.Base as Base

import Control.Monad.State
import qualified Data.IntMap as Map
import Data.Maybe ( fromMaybe )
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

---------------------------------------------------------------------
-- The Z3 Monad

-- | Z3 monad.
--
newtype Z3 a = Z3 (StateT Z3State IO a)
    deriving (Functor, Monad)

instance MonadState Z3State Z3 where
    get = Z3 $ StateT $ \s -> return (s,s)
    put st = Z3 $ StateT $ \_ -> return ((), st)

-- | Existential type. Used to store constants keeping sort info.
--
data AnyAST = forall a. Base.Z3Type a => AnyAST (Base.AST a)

-- | Internal state of Z3 monad.
--
data Z3State
    = Z3State { uniqVal   :: !Uniq
              , context   :: Base.Context
              , consts    :: Map.IntMap AnyAST
              }

-- | Eval a Z3 script.
--
evalZ3 :: Z3 a -> IO a
evalZ3 (Z3 s) = do
    cfg  <- Base.mkConfig
    Base.set_MODEL cfg True
    Base.set_MODEL_PARTIAL cfg False
    ctx  <- Base.mkContext cfg
    evalStateT s Z3State { uniqVal   = 0
                         , context   = ctx
                         , consts    = Map.empty
                         }

-- | Fresh symbol name.
--
fresh :: Z3 (Uniq, String)
fresh = do
    st <- get
    let i = uniqVal st
    put st { uniqVal = i + 1 }
    return (uniqVal st, 'v':show i)

-- | Add a constant of type @Base.AST a@ to the state.
--
addConst :: Base.Z3Type a => Uniq -> Base.AST a -> Z3 ()
addConst u ast = do
    st <- get
    put st { consts = Map.insert u (AnyAST ast) (consts st) }

-- | Get a 'Base.AST' stored in the Z3State.
--
getConst :: forall a. Base.Z3Type a => Uniq -> Z3 (Base.AST a)
getConst u = liftM mlookup $ gets consts
    where mlookup :: Map.IntMap AnyAST -> Base.AST a
          mlookup m =
              maybe (error _UNDEFINED_CONST) extract $ Map.lookup u m

          extract :: AnyAST -> Base.AST a
          extract (AnyAST e) =
              fromMaybe (error _UNDEFINED_CONST) $ Base.castAST e

---------------------------------------------------------------------
-- Lifted Base functions

assertCnstr :: Base.AST Bool -> Z3 ()
assertCnstr b = Z3 . lift . (flip Base.assertCnstr b) =<< gets context

check :: Z3 Base.Result
check = Z3 . lift . Base.check =<< gets context

mkSort :: Base.Z3Type a => Z3 (Base.Sort a)
mkSort = Z3 . lift . Base.mkSort =<< gets context

mkStringSymbol:: String -> Z3 Base.Symbol
mkStringSymbol s = Z3 . lift . (flip Base.mkStringSymbol s) =<< gets context

mkLiteral:: forall a. Base.Z3Scalar a => a -> Z3 (Base.AST a)
mkLiteral l = Z3 . lift . flip Base.mkValue l =<< gets context

mkNot:: Base.AST Bool -> Z3 (Base.AST Bool)
mkNot b = Z3 . lift . flip Base.mkNot b =<< gets context

mkBoolBin:: BoolBinOp -> Base.AST Bool -> Base.AST Bool -> Z3 (Base.AST Bool)
mkBoolBin Xor     b1 b2 = gets context >>= \ctx ->
  Z3 $ lift $ Base.mkXor ctx b1 b2
mkBoolBin Implies b1 b2 = gets context >>= \ctx ->
  Z3 $ lift $ Base.mkImplies ctx b1 b2
mkBoolBin Iff     b1 b2 = gets context >>= \ctx ->
  Z3 $ lift $ Base.mkIff ctx b1 b2

mkBoolMulti:: BoolMultiOp -> [Base.AST Bool] -> Z3 (Base.AST Bool)
mkBoolMulti And bs = Z3 . lift . flip Base.mkAnd bs =<< gets context
mkBoolMulti Or  bs = Z3 . lift . flip Base.mkOr  bs =<< gets context

mkEq:: Base.Z3Scalar a => CmpOpE
                            -> Base.AST a -> Base.AST a
                            -> Z3 (Base.AST Bool)
mkEq Eq  e1 e2 = gets context >>= \ctx -> Z3 $ lift $ Base.mkEq ctx e1 e2
mkEq Neq e1 e2 = gets context >>= \ctx ->
  Z3 $ lift $ (Base.mkNot ctx =<< Base.mkEq ctx e1 e2)

mkCmp:: Base.Z3Num a => CmpOpI
                          -> Base.AST a -> Base.AST a
                          -> Z3 (Base.AST Bool)
mkCmp Le e1 e2 = gets context >>= \ctx -> Z3 $ lift $ Base.mkLe ctx e1 e2
mkCmp Lt e1 e2 = gets context >>= \ctx -> Z3 $ lift $ Base.mkLt ctx e1 e2
mkCmp Ge e1 e2 = gets context >>= \ctx -> Z3 $ lift $ Base.mkGe ctx e1 e2
mkCmp Gt e1 e2 = gets context >>= \ctx -> Z3 $ lift $ Base.mkGt ctx e1 e2

mkConst:: Base.Z3Type a => Base.Symbol -> Base.Sort a -> Z3 (Base.AST a)
mkConst smb srt = gets context >>= \ctx -> Z3 $ lift $ Base.mkConst ctx smb srt

mkUnaryMinus:: Base.Z3Num a => Base.AST a -> Z3 (Base.AST a)
mkUnaryMinus e = Z3 . lift . flip Base.mkUnaryMinus e =<< gets context

mkCRingArith:: Base.Z3Num a => CRingOp -> [Base.AST a] -> Z3 (Base.AST a)
mkCRingArith Add es = Z3 . lift . flip Base.mkAdd es =<< gets context
mkCRingArith Mul es = Z3 . lift . flip Base.mkMul es =<< gets context
mkCRingArith Sub es = Z3 . lift . flip Base.mkSub es =<< gets context

mkIntArith:: IntOp
               -> Base.AST Integer -> Base.AST Integer
               -> Z3 (Base.AST Integer)
mkIntArith Quot e1 e2 = gets context >>= \ctx ->
  Z3 $ lift $ Base.mkDiv ctx e1 e2
mkIntArith Mod  e1 e2 = gets context >>= \ctx ->
  Z3 $ lift $ Base.mkMod ctx e1 e2
mkIntArith Rem  e1 e2 = gets context >>= \ctx ->
  Z3 $ lift $ Base.mkRem ctx e1 e2

mkRealArith:: RealOp
                -> Base.AST Rational -> Base.AST Rational
                -> Z3 (Base.AST Rational)
mkRealArith Div e1 e2 = gets context >>= \ctx ->
  Z3 $ lift $ Base.mkDiv ctx e1 e2

mkIte:: Base.AST Bool -> Base.AST a -> Base.AST a -> Z3 (Base.AST a)
mkIte b e1 e2 = gets context >>= \ctx -> Z3 $ lift $ Base.mkIte ctx b e1 e2

---------------------------------------------------------------------
-- Error messages

_UNDEFINED_CONST :: String
_UNDEFINED_CONST = "Panic! Undefined constant or unexpected\
    \constant sort."

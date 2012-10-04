{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}

-- |
-- Module    : Z3.Lang.Monad
-- Copyright : (c) Iago Abal, 2012
--             (c) David Castro, 2012
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>,
--             David Castro <david.castro.dcp@gmail.com>
-- Stability : experimental

module Z3.Lang.Monad (
    -- * Z3 Monad
      Z3
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

import Z3.Lang.Exprs

import qualified Z3.Base as Base

import Control.Monad.State
import qualified Data.IntMap as Map
import Data.Maybe ( fromMaybe )

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

liftZ3 :: IO a -> Z3 a
liftZ3 = Z3 . lift

liftZ3Op :: (Base.Context -> IO b) -> Z3 b
liftZ3Op f = liftZ3 . f =<< gets context

liftZ3Op2 :: (Base.Context -> a -> IO b) -> a -> Z3 b
liftZ3Op2 f a = gets context >>= \ctx -> liftZ3 (f ctx a)

liftZ3Op3 :: (Base.Context -> a -> b -> IO c) -> a -> b -> Z3 c
liftZ3Op3 f a b = gets context >>= \ctx -> liftZ3 (f ctx a b)
 
liftZ3Op4 :: (Base.Context -> a -> b -> c -> IO d) -> a -> b -> c -> Z3 d
liftZ3Op4 f a b c = gets context >>= \ctx -> liftZ3 (f ctx a b c)

assertCnstr :: Base.AST Bool -> Z3 ()
assertCnstr = liftZ3Op2 Base.assertCnstr

check :: Z3 Base.Result
check = liftZ3Op Base.check

mkSort :: Base.Z3Type a => Z3 (Base.Sort a)
mkSort = liftZ3Op Base.mkSort

mkStringSymbol :: String -> Z3 Base.Symbol
mkStringSymbol = liftZ3Op2 Base.mkStringSymbol

mkLiteral :: forall a. Base.Z3Scalar a => a -> Z3 (Base.AST a)
mkLiteral = liftZ3Op2 Base.mkValue

mkNot :: Base.AST Bool -> Z3 (Base.AST Bool)
mkNot = liftZ3Op2 Base.mkNot

mkBoolBin :: BoolBinOp -> Base.AST Bool -> Base.AST Bool -> Z3 (Base.AST Bool)
mkBoolBin Xor     = liftZ3Op3 Base.mkXor
mkBoolBin Implies = liftZ3Op3 Base.mkImplies
mkBoolBin Iff     = liftZ3Op3 Base.mkIff

mkBoolMulti :: BoolMultiOp -> [Base.AST Bool] -> Z3 (Base.AST Bool)
mkBoolMulti And = liftZ3Op2 Base.mkAnd
mkBoolMulti Or  = liftZ3Op2 Base.mkOr

mkEq :: Base.Z3Scalar a => CmpOpE
                            -> Base.AST a -> Base.AST a
                            -> Z3 (Base.AST Bool)
mkEq Eq  = liftZ3Op3 Base.mkEq
mkEq Neq = liftZ3Op3 mkNeq
  where mkNeq ctx b1 = Base.mkNot ctx <=< Base.mkEq ctx b1

mkCmp :: Base.Z3Num a => CmpOpI
                          -> Base.AST a -> Base.AST a
                          -> Z3 (Base.AST Bool)
mkCmp Le = liftZ3Op3 Base.mkLe
mkCmp Lt = liftZ3Op3 Base.mkLt
mkCmp Ge = liftZ3Op3 Base.mkGe
mkCmp Gt = liftZ3Op3 Base.mkGt

mkConst :: Base.Z3Type a => Base.Symbol -> Base.Sort a -> Z3 (Base.AST a)
mkConst = liftZ3Op3 Base.mkConst

mkUnaryMinus :: Base.Z3Num a => Base.AST a -> Z3 (Base.AST a)
mkUnaryMinus = liftZ3Op2 Base.mkUnaryMinus

mkCRingArith :: Base.Z3Num a => CRingOp -> [Base.AST a] -> Z3 (Base.AST a)
mkCRingArith Add = liftZ3Op2 Base.mkAdd
mkCRingArith Mul = liftZ3Op2 Base.mkMul
mkCRingArith Sub = liftZ3Op2 Base.mkSub

mkIntArith :: IntOp
               -> Base.AST Integer -> Base.AST Integer
               -> Z3 (Base.AST Integer)
mkIntArith Quot = liftZ3Op3 Base.mkDiv
mkIntArith Mod  = liftZ3Op3 Base.mkMod
mkIntArith Rem  = liftZ3Op3 Base.mkRem

mkRealArith :: RealOp
                -> Base.AST Rational -> Base.AST Rational
                -> Z3 (Base.AST Rational)
mkRealArith Div = liftZ3Op3 Base.mkDiv

mkIte :: Base.AST Bool -> Base.AST a -> Base.AST a -> Z3 (Base.AST a)
mkIte = liftZ3Op4 Base.mkIte

---------------------------------------------------------------------
-- Error messages

_UNDEFINED_CONST :: String
_UNDEFINED_CONST = "Panic! Undefined constant or unexpected\
    \constant sort."

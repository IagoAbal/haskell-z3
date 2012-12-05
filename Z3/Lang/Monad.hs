{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}

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
    , Args(..)
    , stdArgs
    , evalZ3With
    , fresh
    , deBruijnIx
    , newQLayout

    -- ** Lifted Z3.Base functions
    , liftZ3
    , assertCnstr
    , check
    , eval
    , getBool
    , getInt
    , getReal
    , getModel
    , getValue
    , mkSort
    , mkStringSymbol
    , mkLiteral
    , mkNot
    , mkBoolBin
    , mkBoolMulti
    , mkPattern
    , mkBound
    , mkForall
    , mkEq
    , mkCmp
    , mkFuncDecl
    , mkApp1
    , mkApp2
    , mkApp3
    , mkApp4
    , mkApp5
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

---------------------------------------------------------------------
-- The Z3 Monad

-- | Z3 monad.
--
newtype Z3 a = Z3 (StateT Z3State IO a)
    deriving (Functor, Monad)

instance MonadState Z3State Z3 where
    get = Z3 $ StateT $ \s -> return (s,s)
    put st = Z3 $ StateT $ \_ -> return ((), st)

-- | Internal state of Z3 monad.
--
data Z3State
    = Z3State { uniqVal :: !Uniq
              , context :: Base.Context
              , qLayout :: !Layout
              }

-- | Eval a Z3 script.
--
evalZ3 :: Z3 a -> IO a
evalZ3 p = evalZ3With p stdArgs

-- | Eval a Z3 script.
--
evalZ3With :: Z3 a -> Args -> IO a
evalZ3With (Z3 s) args = do
    cfg  <- Base.mkConfig
    Base.set_MODEL cfg True
    Base.set_MODEL_PARTIAL cfg False
    Base.setParamValue cfg "WARNING" "false"
    iniConfig cfg args
    ctx  <- Base.mkContext cfg
    evalStateT s Z3State { uniqVal = 0
                         , context = ctx
                         , qLayout = 0
                         }

-- | Fresh symbol name.
--
fresh :: Z3 (Uniq, String)
fresh = do
    st <- get
    let i = uniqVal st
    put st { uniqVal = i + 1 }
    return (uniqVal st, 'v':show i)

-------------------------------------------------
-- Arguments

data Args
  = Args {
      softTimeout :: Maybe Int
    }

stdArgs :: Args
stdArgs 
  = Args {
      softTimeout = Nothing
    }      

iniConfig :: Base.Config -> Args -> IO ()
iniConfig cfg args = do
  Base.setParamValue cfg "SOFT_TIMEOUT" soft_timeout_val
  where soft_timeout_val = show $ maybe 0 id $ softTimeout args 

-------------------------------------------------
-- HOAX-deBruijn conversion

getQLayout :: Z3 Layout
getQLayout = gets qLayout

deBruijnIx :: Layout -> Z3 Int
deBruijnIx k = do lyt <- getQLayout; return $ lyt-k-1

newQLayout :: (Expr a -> Z3 b) -> Z3 b
newQLayout f = do
  lyt <- getQLayout
  incQLayout
  x <- f (Tag lyt)
  decQLayout
  return x
  where incQLayout = modify (\st@Z3State{qLayout} -> st{ qLayout = qLayout+1 })
        decQLayout = modify (\st@Z3State{qLayout} -> st{ qLayout = qLayout-1 })

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

liftZ3Op5 :: (Base.Context -> a -> b -> c -> d -> IO e) -> a -> b -> c -> d -> Z3 e
liftZ3Op5 f a b c d = gets context >>= \ctx -> liftZ3 (f ctx a b c d)

assertCnstr :: Base.AST Bool -> Z3 ()
assertCnstr = liftZ3Op2 Base.assertCnstr

-- | Check satisfiability.
check :: Z3 (Base.Result ())
check = liftZ3Op Base.check

eval :: Base.Model -> Base.AST a -> Z3 (Maybe (Base.AST a))
eval = liftZ3Op3 Base.eval

getBool :: Base.AST Bool -> Z3 (Maybe Bool)
getBool = liftZ3Op2 Base.getBool

getInt :: Base.AST Integer -> Z3 Integer
getInt = liftZ3Op2 Base.getInt

getReal :: Base.AST Rational -> Z3 Rational
getReal = liftZ3Op2 Base.getReal

getModel :: Z3 (Base.Result Base.Model)
getModel = liftZ3Op Base.getModel

getValue :: Base.Z3Type a => Base.AST a -> Z3 a
getValue = liftZ3Op2 Base.getValue

mkSort :: Base.Z3Type a => Z3 (Base.Sort a)
mkSort = liftZ3Op Base.mkSort

mkStringSymbol :: String -> Z3 Base.Symbol
mkStringSymbol = liftZ3Op2 Base.mkStringSymbol

mkLiteral :: forall a. Base.Z3Type a => a -> Z3 (Base.AST a)
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

mkPattern :: Base.Z3Type a => [Base.AST a] -> Z3 Base.Pattern
mkPattern = liftZ3Op2 Base.mkPattern

mkBound :: Base.Z3Type a => Int -> Base.Sort a -> Z3 (Base.AST a)
mkBound = liftZ3Op3 Base.mkBound

mkForall :: Base.Z3Type a => [Base.Pattern] -> Base.Symbol -> Base.Sort a -> Base.AST Bool -> Z3 (Base.AST Bool)
mkForall = liftZ3Op5 Base.mkForall

mkEq :: Base.Z3Type a => CmpOpE
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

mkFuncDecl :: Base.Z3Fun a => Base.Symbol
                                -> Z3 (Base.FuncDecl a)
mkFuncDecl = liftZ3Op2 Base.mkFuncDecl

mkApp1 :: (Base.Z3Type a, Base.Z3Type b)
            => Base.FuncDecl (a -> b)
                -> Base.AST a
                -> Z3 (Base.AST b)
mkApp1 = liftZ3Op3 Base.mkApp1

mkApp2 :: (Base.Z3Type a, Base.Z3Type b, Base.Z3Type c)
            => Base.FuncDecl (a -> b -> c)
                -> Base.AST a -> Base.AST b
                -> Z3 (Base.AST c)
mkApp2 = liftZ3Op4 Base.mkApp2

mkApp3 :: (Base.Z3Type a, Base.Z3Type b, Base.Z3Type c , Base.Z3Type d)
            => Base.FuncDecl (a -> b -> c -> d)
                -> Base.AST a -> Base.AST b -> Base.AST c
                -> Z3 (Base.AST d)
mkApp3 fd a b c
  = gets context >>= \ctx -> liftZ3 $ Base.mkApp3 ctx fd a b c

mkApp4 :: (Base.Z3Type a, Base.Z3Type b, Base.Z3Type c , Base.Z3Type d, Base.Z3Type e)
            => Base.FuncDecl (a -> b -> c -> d -> e)
                -> Base.AST a -> Base.AST b -> Base.AST c -> Base.AST d
                -> Z3 (Base.AST e)
mkApp4 fd a b c d
  = gets context >>= \ctx -> liftZ3 $ Base.mkApp4 ctx fd a b c d

mkApp5 :: (Base.Z3Type a, Base.Z3Type b, Base.Z3Type c , Base.Z3Type d, Base.Z3Type e, Base.Z3Type f)
            => Base.FuncDecl (a -> b -> c -> d -> e -> f)
                -> Base.AST a -> Base.AST b -> Base.AST c -> Base.AST d -> Base.AST e
                -> Z3 (Base.AST f)
mkApp5 fd a b c d e
  = gets context >>= \ctx -> liftZ3 $ Base.mkApp5 ctx fd a b c d e

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

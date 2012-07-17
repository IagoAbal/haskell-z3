{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- |
-- Module    : Z3.Monad
-- Copyright : (c) Iago Abal, 2012
--             (c) David Castro, 2012
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>, 
--             David Castro <david.castro.dcp@gmail.com>


module Z3.Monad (
    -- * Z3 Monad
      Z3
    , evalZ3

    -- * Satisfiability result
    , Base.Result(..)

    -- * Z3 actions
    , var
    , assert
    , let_
    , check

    , module Z3.Exprs

    ) where

import qualified Z3.Base as Base
import Z3.Exprs
import Z3.Exprs.Internal

import Control.Monad
import Control.Monad.State.Strict
import Data.Maybe ( fromMaybe )
import qualified Data.IntMap as Map

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
{- TODO: I believe that getConst return type should be Z3 (Base.AST a)
         instead. If Map.lookup returns nothing then we "panic" because
         an undefined constant, if Base.castAST returns nothing then we
         panic because of type mismatch. We have the duty to guarantee
         that none of these errors would ever happen.
          - Iago
-}
getConst :: forall a. Base.Z3Type a => Uniq -> Z3 (Maybe (Base.AST a))
getConst u = liftM mlookup (gets consts)
    where mlookup :: Map.IntMap AnyAST -> Maybe (Base.AST a)
          mlookup m = Map.lookup u m >>= \(AnyAST e) -> Base.castAST e

---------------------------------------------------------------------
-- Constructing expressions

-- | Declare skolem variables.
--
var :: forall a. IsTy a => Z3 (Expr a)
var = do
    ctx <- gets context
    (u, str) <- fresh
    smb <- mkStringSymbol_ ctx str
    (srt :: Base.Sort (TypeZ3 a)) <- Z3 . lift $ Base.mkSort ctx
    addConst u =<< mkConst_ ctx smb srt
    return $ Const u

-- | Make assertion in current context.
--
assert :: Expr Bool -> Z3 ()
assert = join . liftM2 assertCnstr_ (gets context) . compile

-- | Introduce an auxiliary declaration to name a given expression.
--
-- If you really want sharing use this instead of Haskell's /let/.
--
let_ :: IsScalar a => Expr a -> Z3 (Expr a)
let_ e = do
  aux <- var
  assert (aux ==* e)
  return aux

-- | Check current context.
--
check :: Z3 Base.Result
check = check_ =<< gets context

-- | Create a 'Base.AST' from a 'Expr'.
--
compile :: IsTy a => Expr a -> Z3 (Base.AST (TypeZ3 a))
compile (Lit a)
    = flip mkLiteral_ (toZ3Type a) =<< gets context
compile (Const u)
    = liftM (fromMaybe $ error uNDEFINED_CONST) $ getConst u
compile (Not b)
    = do ctx <- gets context
         b'  <- compile b
         mkNot_ ctx b'
compile (BoolBin op e1 e2)
    = do ctx <- gets context
         e1' <- compile e1
         e2' <- compile e2
         mkBoolBin_ ctx op e1' e2'
compile (BoolMulti op es)
    = do ctx <- gets context
         es' <- mapM compile es
         mkBoolMulti_ ctx op es'
compile (Neg e)
    = do ctx <- gets context
         e'  <- compile e
         mkUnaryMinus_ ctx e'
compile (CRingArith op es)
    = do ctx <- gets context
         es' <- mapM compile es
         mkCRingArith_ ctx op es'
compile (IntArith op e1 e2)
    = do ctx <- gets context
         e1' <- compile e1
         e2' <- compile e2
         mkIntArith_ ctx op e1' e2'
compile (RealArith op e1 e2)
    = do ctx <- gets context
         e1' <- compile e1
         e2' <- compile e2
         mkRealArith_ ctx op e1' e2'
compile (CmpE op e1 e2)
    = do ctx <- gets context
         e1' <- compile e1
         e2' <- compile e2
         mkEq_ ctx op e1' e2'
compile (CmpI op e1 e2)
    = do ctx <- gets context
         e1' <- compile e1
         e2' <- compile e2
         mkCmp_ ctx op e1' e2'
compile (Ite b e1 e2)
    = do ctx <- gets context
         b'  <- compile b
         e1' <- compile e1
         e2' <- compile e2
         mkIte_ ctx b' e1' e2'

---------------------------------------------------------------------
-- Internal lifted Base functions

assertCnstr_ :: Base.Context -> Base.AST Bool -> Z3 ()
assertCnstr_ ctx = Z3 . lift . Base.assertCnstr ctx

check_ :: Base.Context -> Z3 Base.Result
check_ = Z3 . lift . Base.check

mkStringSymbol_ :: Base.Context -> String -> Z3 Base.Symbol
mkStringSymbol_ ctx = Z3 . lift . Base.mkStringSymbol ctx

mkLiteral_ :: forall a. Base.Z3Scalar a => Base.Context -> a -> Z3 (Base.AST a)
mkLiteral_ ctx = Z3 . lift . Base.mkValue ctx

mkNot_ :: Base.Context -> Base.AST Bool -> Z3 (Base.AST Bool)
mkNot_ ctx = Z3 . lift . Base.mkNot ctx

mkBoolBin_ :: Base.Context -> BoolBinOp ->
    Base.AST Bool -> Base.AST Bool -> Z3 (Base.AST Bool)
mkBoolBin_ ctx Xor     b1 = Z3 . lift . Base.mkXor ctx b1
mkBoolBin_ ctx Implies b1 = Z3 . lift . Base.mkImplies ctx b1
mkBoolBin_ ctx Iff     b1 = Z3 . lift . Base.mkIff ctx b1

mkBoolMulti_ :: Base.Context -> BoolMultiOp ->
    [Base.AST Bool] -> Z3 (Base.AST Bool)
mkBoolMulti_ ctx And = Z3 . lift . Base.mkAnd ctx
mkBoolMulti_ ctx Or  = Z3 . lift . Base.mkAnd ctx

mkEq_ :: Base.Z3Scalar a => Base.Context -> CmpOpE ->
    Base.AST a -> Base.AST a -> Z3 (Base.AST Bool)
mkEq_ ctx Eq  e1 = Z3 . lift . Base.mkEq ctx e1
mkEq_ ctx Neq e1 = Z3 . lift . (Base.mkNot ctx <=< Base.mkEq ctx e1)

mkCmp_ :: Base.Z3Num a => Base.Context -> CmpOpI ->
    Base.AST a -> Base.AST a -> Z3 (Base.AST Bool)
mkCmp_ ctx Le e1 = Z3 . lift . Base.mkLe ctx e1
mkCmp_ ctx Lt e1 = Z3 . lift . Base.mkLt ctx e1
mkCmp_ ctx Ge e1 = Z3 . lift . Base.mkGe ctx e1
mkCmp_ ctx Gt e1 = Z3 . lift . Base.mkGt ctx e1

mkConst_ :: Base.Z3Type a => Base.Context
              -> Base.Symbol -> Base.Sort a -> Z3 (Base.AST a)
mkConst_ ctx smb = Z3 . lift . Base.mkConst ctx smb

mkUnaryMinus_ :: Base.Z3Num a => Base.Context -> Base.AST a -> Z3 (Base.AST a)
mkUnaryMinus_ ctx = Z3 . lift . Base.mkUnaryMinus ctx

mkCRingArith_ :: Base.Z3Num a => Base.Context
                    -> CRingOp -> [Base.AST a] -> Z3 (Base.AST a)
mkCRingArith_ ctx Add = Z3 . lift . Base.mkAdd ctx
mkCRingArith_ ctx Mul = Z3 . lift . Base.mkMul ctx
mkCRingArith_ ctx Sub = Z3 . lift . Base.mkSub ctx

mkIntArith_ :: Base.Context
                -> IntOp
                -> Base.AST Integer -> Base.AST Integer
                -> Z3 (Base.AST Integer)
mkIntArith_ ctx Quot e1 = Z3 . lift . Base.mkDiv ctx e1
mkIntArith_ ctx Mod  e1 = Z3 . lift . Base.mkMod ctx e1
mkIntArith_ ctx Rem  e1 = Z3 . lift . Base.mkRem ctx e1

mkRealArith_ :: Base.Context
                  -> RealOp
                  -> Base.AST Rational -> Base.AST Rational
                  -> Z3 (Base.AST Rational)
mkRealArith_ ctx Div e1 = Z3 . lift . Base.mkDiv ctx e1

mkIte_ :: Base.Context
            -> Base.AST Bool
            -> Base.AST a -> Base.AST a
            -> Z3 (Base.AST a)
mkIte_ ctx b e1 = Z3 . lift . Base.mkIte ctx b e1

---------------------------------------------------------------------
-- Error messages

uNDEFINED_CONST :: String
uNDEFINED_CONST = "Panic! Undefined constant or unexpected\
    \constant sort."

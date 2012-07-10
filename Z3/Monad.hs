{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

{-# LANGUAGE ExistentialQuantification #-}
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
    , Z3State
    , evalZ3

    -- * Z3 actions
    , decl
    , assert
    , let_
    , check

    ) where

import qualified Z3.Base as Base
import Z3.Exprs ( (==*) )
import Z3.Exprs.Internal
import Z3.Types

import Control.Monad
import Control.Monad.State.Strict
import Data.Maybe ( fromMaybe )
import qualified Data.IntMap as Map

---------------------------------------------------------------------
-- The Z3 Monad

-- | Z3 Monad type
--
newtype Z3 a = Z3 (StateT Z3State IO a)
    deriving (Functor, Monad)

instance MonadState Z3State Z3 where
    get = Z3 $ StateT $ \s -> return (s,s)
    put st = Z3 $ StateT $ \_ -> return ((), st)

-- | Existential type. Used to store constants keeping sort info.
--
data AnyAST = forall a. Z3Type a => AnyAST (Base.AST a)

-- | Z3 monad State type.
--
data Z3State
    = Z3State { uniqVal   :: Uniq
              , context   :: Base.Context
              , consts    :: Map.IntMap AnyAST
              }

-- | evalStateT with empty initial state for the Z3 Monad
--
evalZ3 :: Z3 a -> IO a
evalZ3 (Z3 s) = do
    cfg  <- Base.mkConfig
    ctx  <- Base.mkContext cfg
    evalStateT s Z3State { uniqVal   = 0
                         , context   = ctx
                         , consts    = Map.empty
                         }

-- | Fresh string
--
fresh :: Z3 (Uniq, String)
fresh = do
    st <- get
    let i = uniqVal st
    put st { uniqVal = i + 1 }
    return (uniqVal st, 'v':show i)

-- | Add a constant of type AST a to the state.
--
addConst :: (Z3Type a) => Uniq -> Base.AST a -> Z3 ()
addConst u expr = do
    st <- get
    put st { consts = Map.insert u (AnyAST expr) (consts st) }

-- | Get a Base.AST stored in the Z3State
--
getConst :: forall a. (Z3Type a) => Uniq -> Z3 (Maybe (Base.AST a))
getConst u = liftM mlookup (gets consts)
    where mlookup :: Map.IntMap AnyAST -> Maybe (Base.AST a)
          mlookup m = Map.lookup u m >>= \(AnyAST e) -> Base.castAST e

---------------------------------------------------------------------
-- Constructing expressions

-- | Declare constants
--
decl :: forall a. Z3Type a => Z3 (Expr a)
decl = do
    ctx <- gets context
    (u, str) <-  fresh
    smb <- mkStringSymbol_ ctx str
    (srt :: Base.Sort a) <- mkSort_ ctx
    addConst u =<< mkConst_ ctx smb srt
    return $ Const u

-- | Make assertion in current context
--
assert :: Z3Type a => Expr a -> Z3 ()
assert = join . liftM2 assertCnstr_ (gets context) . compile

-- | Introduce an auxiliary declaration to name a given expression.
--
let_ :: Z3Type a => Expr a -> Z3 (Expr a)
let_ e = do
  aux <- decl
  assert (aux ==* e)
  return aux

-- | Check current context
--
check :: Z3 (Maybe Bool)
check = liftM fromResult . check_ =<< gets context
    where fromResult :: Base.Result -> Maybe Bool
          fromResult Base.Unsat = Just False
          fromResult Base.Sat   = Just True
          fromResult _          = Nothing

-- | Create a Base.AST from a Expr
--
compile :: Z3Type a => Expr a -> Z3 (Base.AST a)
compile (Lit a)
    = flip mkLiteral_ a =<< gets context
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
compile (Cmp op e1 e2)
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

assertCnstr_ :: Base.Context -> Base.AST a -> Z3 ()
assertCnstr_ ctx = Z3 . lift . Base.assertCnstr ctx

check_ :: Base.Context -> Z3 Base.Result
check_ = Z3 . lift . Base.check

mkBoolBin_ :: Base.Context -> BoolBinOp ->
    Base.AST Bool -> Base.AST Bool -> Z3 (Base.AST Bool)
mkBoolBin_ ctx Xor b1 = Z3 . lift . Base.mkXor ctx b1
mkBoolBin_ ctx Implies b1 = Z3 . lift . Base.mkImplies ctx b1
mkBoolBin_ ctx Iff b1 = Z3 . lift . Base.mkIff ctx b1

mkBoolMulti_ :: Base.Context -> BoolMultiOp ->
    [Base.AST Bool] -> Z3 (Base.AST Bool)
mkBoolMulti_ ctx And = Z3 . lift . Base.mkAnd ctx
mkBoolMulti_ ctx Or  = Z3 . lift . Base.mkAnd ctx

mkCmp_ :: (Z3Type a) => Base.Context -> CmpOp a ->
    Base.AST a -> Base.AST a -> Z3 (Base.AST Bool)
mkCmp_ ctx Eq  e1 = Z3 . lift . Base.mkEq ctx e1
mkCmp_ ctx Neq e1 = Z3 . lift . (Base.mkNot ctx <=< Base.mkEq ctx e1)
mkCmp_ ctx Le  e1 = Z3 . lift . Base.mkLe ctx e1
mkCmp_ ctx Lt  e1 = Z3 . lift . Base.mkLt ctx e1
mkCmp_ ctx Ge  e1 = Z3 . lift . Base.mkGe ctx e1
mkCmp_ ctx Gt  e1 = Z3 . lift . Base.mkGt ctx e1

mkConst_ :: Base.Context -> Base.Symbol -> Base.Sort a -> Z3 (Base.AST a)
mkConst_ ctx smb = Z3 . lift . Base.mkConst ctx smb

mkCRingArith_ :: Z3Num a => Base.Context -> CRingOp ->
    [Base.AST a] -> Z3 (Base.AST a)
mkCRingArith_ ctx Add = Z3 . lift . Base.mkAdd ctx
mkCRingArith_ ctx Mul = Z3 . lift . Base.mkMul ctx
mkCRingArith_ ctx Sub = Z3 . lift . Base.mkSub ctx

mkIntArith_ :: Z3Int a => Base.Context -> IntOp ->
    Base.AST a -> Base.AST a -> Z3 (Base.AST a)
mkIntArith_ ctx Quot e1 = Z3 . lift . Base.mkDiv ctx e1
mkIntArith_ ctx Mod  e1 = Z3 . lift . Base.mkMod ctx e1
mkIntArith_ ctx Rem  e1 = Z3 . lift . Base.mkRem ctx e1

mkIte_ :: Base.Context -> Base.AST Bool ->
    Base.AST a -> Base.AST a -> Z3 (Base.AST a)
mkIte_ ctx b e1 = Z3 . lift . Base.mkIte ctx b e1


mkRealArith_ :: Z3Real a => Base.Context -> RealOp ->
    Base.AST a -> Base.AST a -> Z3 (Base.AST a)
mkRealArith_ ctx Div e1 = Z3 . lift . Base.mkDiv ctx e1

mkSort_ :: forall a. Z3Type a => Base.Context -> Z3 (Base.Sort a) 
mkSort_
    | SBool <- sortZ3 (TY :: TY a) = Z3 . lift . Base.mkBoolSort
    | SInt  <- sortZ3 (TY :: TY a) = Z3 . lift . Base.mkIntSort
    | SReal <- sortZ3 (TY :: TY a) = Z3 . lift . Base.mkRealSort
    | otherwise                    = error pANIC_SORTS 

mkLiteral_ :: forall a. Z3Type a => Base.Context -> a -> Z3 (Base.AST a)
mkLiteral_ ctx
    | SBool <- sortZ3 (TY :: TY a) = mkBool_
    | SInt  <- sortZ3 (TY :: TY a) = Z3 . lift . Base.mkInt  ctx
    | SReal <- sortZ3 (TY :: TY a) = Z3 . lift . Base.mkReal ctx
    | otherwise                    = error pANIC_SORTS 
    where mkBool_ :: Bool -> Z3 (Base.AST Bool)
          mkBool_ True  = Z3 . lift $ Base.mkTrue ctx
          mkBool_ False = Z3 . lift $ Base.mkFalse ctx

mkNot_ :: Base.Context -> Base.AST Bool -> Z3 (Base.AST Bool)
mkNot_ ctx = Z3 . lift . Base.mkNot ctx

mkStringSymbol_ :: Base.Context -> String -> Z3 Base.Symbol
mkStringSymbol_ ctx = Z3 . lift . Base.mkStringSymbol ctx

mkUnaryMinus_ :: (Z3Num a) => Base.Context -> Base.AST a -> Z3 (Base.AST a)
mkUnaryMinus_ ctx = Z3 . lift . Base.mkUnaryMinus ctx

---------------------------------------------------------------------
-- Error messages

pANIC_SORTS :: String
pANIC_SORTS = "Panic! A type of class Z3Type is not instance\
    \of the corresponding subclass (Z3Int, Z3Real, ...)"

uNDEFINED_CONST :: String
uNDEFINED_CONST = "Panic! Undefined constant or unexpected\
    \constant sort."

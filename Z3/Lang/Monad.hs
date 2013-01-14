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
    , delModel
    , withModel
    , getValue
    , mkSort
    , mkStringSymbol
    , mkBoolSort
    , mkIntSort
    , mkRealSort
    , mkLiteral
    , mkNot
    , mkBoolBin
    , mkBoolMulti
    , mkNumeral
    , mkInt
    , mkReal
    , mkPattern
    , mkBound
    , mkForall
    , mkExists
    , mkQuant
    , mkEq
    , mkCmp
    , mkFuncDecl
    , mkApp
    , mkConst
    , mkTrue
    , mkFalse
    , mkUnaryMinus
    , mkCRingArith
    , mkIntArith
    , mkRealArith
    , mkIte
    , pop
    , push
    , showContext
    , showModel

    -- * Satisfiability result
    , Base.Result

    ) where

import Z3.Lang.Exprs

import qualified Z3.Base as Base

import Control.Applicative ( Applicative )
import Control.Monad.State
import Data.Traversable ( traverse )

---------------------------------------------------------------------
-- The Z3 Monad

-- | Z3 monad.
--
newtype Z3 a = Z3 (StateT Z3State IO a)
    deriving (Functor, Applicative, Monad, MonadState Z3State)

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
evalZ3 = evalZ3With stdArgs

-- | Eval a Z3 script.
--
evalZ3With :: Args -> Z3 a -> IO a
evalZ3With args (Z3 s) =
  Base.withConfig $ \cfg -> do
    Base.set_MODEL cfg True
    Base.set_MODEL_PARTIAL cfg False
--    Base.setParamValue cfg "WARNING" "false"
    iniConfig cfg args
    Base.withContext cfg $ \ctx ->
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
        -- ^ soft timeout (in milliseconds)
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

assertCnstr :: Base.AST -> Z3 ()
assertCnstr = liftZ3Op2 Base.assertCnstr

-- | Check satisfiability.
check :: Z3 Base.Result
check = liftZ3Op Base.check

eval :: Base.Model -> Base.AST -> Z3 (Maybe Base.AST)
eval = liftZ3Op3 Base.eval

getBool :: Base.AST -> Z3 (Maybe Bool)
getBool = liftZ3Op2 Base.getBool

push :: Z3 ()
push = liftZ3Op Base.push

pop :: Int -> Z3 ()
pop = liftZ3Op2 Base.pop

getInt :: Base.AST -> Z3 Integer
getInt = liftZ3Op2 Base.getInt

getReal :: Base.AST -> Z3 Rational
getReal = liftZ3Op2 Base.getReal

getModel :: Z3 (Base.Result, Maybe Base.Model)
getModel = liftZ3Op Base.getModel

delModel :: Base.Model -> Z3 ()
delModel = liftZ3Op2 Base.delModel

withModel :: (Base.Model -> Z3 a) -> Z3 (Maybe a)
withModel f = do
 (_,m) <- getModel
 r <- traverse f m
 _ <- traverse delModel m
 return r

showModel :: Z3 (Maybe String)
showModel = withModel (liftZ3Op2 Base.showModel)

showContext :: Z3 String
showContext = do
  c <- gets context
  liftZ3 $ Base.showContext c

mkStringSymbol :: String -> Z3 Base.Symbol
mkStringSymbol = liftZ3Op2 Base.mkStringSymbol

mkBoolSort :: Z3 Base.Sort
mkBoolSort = liftZ3Op Base.mkBoolSort

mkIntSort :: Z3 Base.Sort
mkIntSort = liftZ3Op Base.mkIntSort

mkRealSort :: Z3 Base.Sort
mkRealSort = liftZ3Op Base.mkRealSort

mkNot :: Base.AST -> Z3 Base.AST
mkNot = liftZ3Op2 Base.mkNot

mkBoolBin :: BoolBinOp -> Base.AST -> Base.AST -> Z3 Base.AST
mkBoolBin Xor     = liftZ3Op3 Base.mkXor
mkBoolBin Implies = liftZ3Op3 Base.mkImplies
mkBoolBin Iff     = liftZ3Op3 Base.mkIff

mkBoolMulti :: BoolMultiOp -> [Base.AST] -> Z3 Base.AST
mkBoolMulti And      = liftZ3Op2 Base.mkAnd
mkBoolMulti Or       = liftZ3Op2 Base.mkOr

mkNumeral :: String -> Base.Sort -> Z3 Base.AST
mkNumeral = liftZ3Op3 Base.mkNumeral

mkInt  :: Integral a => a -> Z3 Base.AST
mkInt = liftZ3Op2 Base.mkInt

mkReal :: Real r => r -> Z3 Base.AST
mkReal = liftZ3Op2 Base.mkReal

mkPattern :: [Base.AST] -> Z3 Base.Pattern
mkPattern = liftZ3Op2 Base.mkPattern

mkBound :: Int -> Base.Sort -> Z3 Base.AST
mkBound = liftZ3Op3 Base.mkBound

mkForall :: [Base.Pattern]
            -> [Base.Symbol] -> [Base.Sort]
            -> Base.AST -> Z3 Base.AST
mkForall = liftZ3Op5 Base.mkForall

mkExists :: [Base.Pattern]
            -> [Base.Symbol] -> [Base.Sort]
            -> Base.AST -> Z3 Base.AST
mkExists = liftZ3Op5 Base.mkExists

mkQuant :: Quantifier
           -> [Base.Pattern]
           -> [Base.Symbol] -> [Base.Sort]
           -> Base.AST -> Z3 Base.AST
mkQuant ForAll = mkForall
mkQuant Exists = mkExists

mkEq :: CmpOpE -> [Base.AST] -> Z3 Base.AST
mkEq Distinct = liftZ3Op2 Base.mkDistinct
mkEq Neq      = mkNot <=< mkEq Eq
mkEq Eq       = doMkEq
  where doMkEq [e1,e2]
          = (liftZ3Op3 Base.mkEq) e1 e2
        doMkEq es
          | length es < 2 = error "Z3.Lang.Monad.mkEq:\
              \ Invalid number of parameters."
          | otherwise     = join $ liftM (mkBoolMulti And) doEqs
          where doEqs = mapM (uncurry $ liftZ3Op3 Base.mkEq) pairs
                pairs = init $ zip es $ tail $ cycle es

mkCmp :: CmpOpI -> Base.AST -> Base.AST -> Z3 Base.AST
mkCmp Le = liftZ3Op3 Base.mkLe
mkCmp Lt = liftZ3Op3 Base.mkLt
mkCmp Ge = liftZ3Op3 Base.mkGe
mkCmp Gt = liftZ3Op3 Base.mkGt

mkFuncDecl :: Base.Symbol -> [Base.Sort] -> Base.Sort -> Z3 Base.FuncDecl
mkFuncDecl = liftZ3Op4 Base.mkFuncDecl

mkApp :: Base.FuncDecl -> [Base.AST] -> Z3 Base.AST
mkApp = liftZ3Op3 Base.mkApp

mkConst :: Base.Symbol -> Base.Sort -> Z3 Base.AST
mkConst = liftZ3Op3 Base.mkConst

mkTrue :: Z3 Base.AST
mkTrue = liftZ3Op Base.mkTrue

mkFalse :: Z3 Base.AST
mkFalse = liftZ3Op Base.mkFalse

mkUnaryMinus :: Base.AST -> Z3 Base.AST
mkUnaryMinus = liftZ3Op2 Base.mkUnaryMinus

mkCRingArith :: CRingOp -> [Base.AST] -> Z3 Base.AST
mkCRingArith Add = liftZ3Op2 Base.mkAdd
mkCRingArith Mul = liftZ3Op2 Base.mkMul
mkCRingArith Sub = liftZ3Op2 Base.mkSub

mkIntArith :: IntOp -> Base.AST -> Base.AST -> Z3 Base.AST
mkIntArith Quot = liftZ3Op3 Base.mkDiv
mkIntArith Mod  = liftZ3Op3 Base.mkMod
mkIntArith Rem  = liftZ3Op3 Base.mkRem

mkRealArith :: RealOp -> Base.AST -> Base.AST -> Z3 Base.AST
mkRealArith Div = liftZ3Op3 Base.mkDiv

mkIte :: Base.AST -> Base.AST -> Base.AST -> Z3 Base.AST
mkIte = liftZ3Op4 Base.mkIte

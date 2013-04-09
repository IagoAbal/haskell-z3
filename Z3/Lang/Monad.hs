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

-- TODO Discuss integration of pretty-printing functions from Z3.Monad
module Z3.Lang.Monad (
    -- * Z3 Monad
      Z3
    , Z3State(..)
    , evalZ3
    , Args(..)
    , stdArgs
    , Logic(..)
    , module Z3.Opts
    , evalZ3With
    , fresh
    , deBruijnIx
    , newQLayout

    -- ** Lifted Z3.Base functions
    , getValue
    , mkSort
    , mkLiteral
    , mkBoolBin
    , mkBoolMulti
    , mkQuant
    , mkEq
    , mkCmp
    , mkCRingArith
    , mkIntArith
    , mkRealArith

    -- * Satisfiability result
    , Base.Result

    ) where

import Z3.Lang.Exprs

import qualified Z3.Base as Base
import qualified Z3.Monad as MonadZ3
import Z3.Monad ( MonadZ3(..), Logic(..) )
import Z3.Opts

import Control.Applicative ( Applicative )
import Control.Monad.State
import qualified Data.Traversable as T

---------------------------------------------------------------------
-- The Z3 Monad

-- | Z3 monad.
newtype Z3 a = Z3 (StateT Z3State IO a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadState Z3State)

instance MonadZ3 Z3 where
  getSolver  = Z3 $ gets solver
  getContext = Z3 $ gets context

-- | Internal state of Z3 monad.
data Z3State
    = Z3State { uniqVal :: !Uniq
              , context :: Base.Context
              , solver :: Maybe Base.Solver
              , qLayout :: !Layout
              }

-- | Eval a Z3 script.
evalZ3 :: Z3 a -> IO a
evalZ3 = evalZ3With stdArgs

-- | Eval a Z3 script.
evalZ3With :: Args -> Z3 a -> IO a
evalZ3With args (Z3 s) =
  Base.withConfig $ \cfg -> do
    setArgs cfg args
    Base.withContext cfg $ \ctx ->
      do mbSolver <- T.mapM (Base.mkSolverForLogic ctx) $ logic args
         evalStateT s Z3State { uniqVal = 0
                              , context = ctx
                              , solver = mbSolver
                              , qLayout = 0
                              }

-- | Fresh symbol name.
fresh :: Z3 (Uniq, String)
fresh = do
    st <- get
    let i = uniqVal st
    put st { uniqVal = i + 1 }
    return (uniqVal st, 'v':show i)

-------------------------------------------------
-- Arguments

{-# WARNING logic
          "New Z3 API support is still incomplete and fragile: \
          \you may experience segmentation faults!"
  #-}

data Args
  = Args {
      logic        :: Maybe Logic
        -- ^ the logic to use; see <http://smtlib.cs.uiowa.edu/logics.html>
    ,  softTimeout :: Maybe Int
        -- ^ soft timeout (in milliseconds)
    , options      :: Opts
        -- ^ Z3 options
    }

stdArgs :: Args
stdArgs
  = Args {
      logic       = Nothing
    , softTimeout = Nothing
    , options     = stdOpts +? opt "MODEL" True
                            +? opt "MODEL_COMPLETION" True
    }

setArgs :: Base.Config -> Args -> IO ()
setArgs cfg args = do
  Base.setParamValue cfg "SOFT_TIMEOUT" soft_timeout_val
  setOpts cfg $ options args
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

mkBoolBin :: BoolBinOp -> Base.AST -> Base.AST -> Z3 Base.AST
mkBoolBin Xor     = MonadZ3.mkXor
mkBoolBin Implies = MonadZ3.mkImplies
mkBoolBin Iff     = MonadZ3.mkIff

mkBoolMulti :: BoolMultiOp -> [Base.AST] -> Z3 Base.AST
mkBoolMulti And      = MonadZ3.mkAnd
mkBoolMulti Or       = MonadZ3.mkOr

mkQuant :: Quantifier
           -> [Base.Pattern]
           -> [Base.Symbol] -> [Base.Sort]
           -> Base.AST -> Z3 Base.AST
mkQuant ForAll = MonadZ3.mkForall
mkQuant Exists = MonadZ3.mkExists

mkEq :: CmpOpE -> [Base.AST] -> Z3 Base.AST
mkEq Distinct = MonadZ3.mkDistinct
mkEq Eq       = doMkEq
  where doMkEq [e1,e2]
          = MonadZ3.mkEq e1 e2
        doMkEq es
          | length es < 2 = error "Z3.Lang.Monad.mkEq:\
              \ Invalid number of parameters."
          | otherwise     = join $ liftM (mkBoolMulti And) doEqs
          where doEqs = mapM (uncurry $ MonadZ3.mkEq) pairs
                pairs = init $ zip es $ tail $ cycle es

mkCmp :: CmpOpI -> Base.AST -> Base.AST -> Z3 Base.AST
mkCmp Le = MonadZ3.mkLe
mkCmp Lt = MonadZ3.mkLt
mkCmp Ge = MonadZ3.mkGe
mkCmp Gt = MonadZ3.mkGt

mkCRingArith :: CRingOp -> [Base.AST] -> Z3 Base.AST
mkCRingArith Add = MonadZ3.mkAdd
mkCRingArith Mul = MonadZ3.mkMul
mkCRingArith Sub = MonadZ3.mkSub

mkIntArith :: IntOp -> Base.AST -> Base.AST -> Z3 Base.AST
mkIntArith Quot = MonadZ3.mkDiv
mkIntArith Mod  = MonadZ3.mkMod
mkIntArith Rem  = MonadZ3.mkRem

mkRealArith :: RealOp -> Base.AST -> Base.AST -> Z3 Base.AST
mkRealArith Div = MonadZ3.mkDiv


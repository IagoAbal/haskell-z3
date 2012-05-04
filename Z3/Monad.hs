{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Z3.Monad (
      Z3
    , evalZ3

    , decl

    ) where

import Z3.Base hiding ( Sort )
import Z3.Exprs.Internal
import Z3.Types

import Control.Monad.State.Strict
import Data.IORef
import qualified Data.Map as Map

data Exp = forall a. Z3Type a => Exp (AST a)

-- | The Z3 Monad
data Z3State
    = Z3State { uniqRef   :: IORef Uniq
              , smbSupply :: IORef [String]
              , context   :: Context
              , consts    :: Map.Map Uniq Exp
              }

newtype Z3 a = Z3 (StateT Z3State IO a)
    deriving (Functor, Monad)

instance MonadState Z3State Z3 where
    get = Z3 $ StateT $ \s -> return (s,s)
    put st = Z3 $ StateT $ \_ -> return ((), st)

evalZ3 :: Z3 a -> IO a
evalZ3 (Z3 s) = do
    uref <- newIORef (Uniq 0)
    sref <- newIORef smbGen
    cfg  <- mkConfig
    ctx  <- mkContext cfg
    evalStateT s Z3State { uniqRef   = uref
                         , smbSupply = sref
                         , context   = ctx
                         , consts    = Map.empty
                         }
  where smbGen :: [String]
        smbGen =  [ [c] | c <- ['a' .. 'z'] ] 
               ++ [ c:(show i) | c <- ['a' .. 'z'], i <- [1 :: Integer ..] ]

newUniq :: Z3 Uniq
newUniq = do
    st <- return . uniqRef =<< get
    modifyZ3Ref st (\(Uniq i) -> Uniq $ i + 1)

uniqSymbol :: Z3 String
uniqSymbol = do
    st <- gets smbSupply
    str <- Z3 $ lift $ readIORef st
    Z3 $ lift $ writeIORef st (tail str)
    return (head str)

modifyZ3Ref :: IORef a -> (a -> a) -> Z3 a
modifyZ3Ref r f = Z3 $ lift (readIORef r) >>= \a -> do
    lift $ modifyIORef r f
    return a

-- | Constructing expressions

-- * Declare constants
decl :: forall a.(Z3Type a) => Z3 (Expr a)
decl = do
    st  <- get
    u   <- newUniq
    let ctx = context st
    str <- uniqSymbol
    smb <- Z3 $ lift $ mkStringSymbol ctx str
    srt <- cnstSort ctx
    (c   :: AST a ) <- Z3 $ lift $ mkConst ctx smb srt
    put st { consts = Map.insert u (Exp c) (consts st) }
    return $ Const u
  where ty = TY :: TY a
        cnstSort ctx
            | SBool <- sortZ3 ty
                = Z3 $ lift $ mkBoolSort ctx
            | SInt  <- sortZ3 ty
                = Z3 $ lift $ mkIntSort ctx
            | SReal <- sortZ3 ty
                = Z3 $ lift $ mkRealSort ctx
            | otherwise
                = error "Panic! The impossible happened. \
                        \A Z3Type of sort Int/Real is not\
                        \instance of Z3Int/Z3Real"

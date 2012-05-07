{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- |
-- Module    : Z3.Monad
-- Copyright : (c) Iago Abal, 2011 
--             (c) David Castro, 2011
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>, 
--             David Castro <david.castro.dcp@gmail.com>


module Z3.Monad (
      Z3
    , Z3State

    , evalZ3

    , decl

    ) where

import qualified Z3.Base as Base
import Z3.Exprs.Internal
import Z3.Types

import Control.Monad.State.Strict
import Data.IORef
import qualified Data.Map as Map

-- | Existential type. Used to store constants keeping sort info.
data Exp = forall a. Z3Type a => Exp (Base.AST a)

-- | State for the Z3 Monad
data Z3State
    = Z3State { uniqRef   :: IORef Uniq
              , smbSupply :: [String]
              , context   :: Base.Context
              , consts    :: Map.Map Uniq Exp
              }

-- | The Z3 Monad
newtype Z3 a = Z3 (StateT Z3State IO a)
    deriving (Functor, Monad)

instance MonadState Z3State Z3 where
    get = Z3 $ StateT $ \s -> return (s,s)
    put st = Z3 $ StateT $ \_ -> return ((), st)

-- | evalStateT with empty initial state for the Z3 Monad
evalZ3 :: Z3 a -> IO a
evalZ3 (Z3 s) = do
    uref <- newIORef (Uniq 0)
    -- sref <- newIORef smbGen
    cfg  <- Base.mkConfig
    ctx  <- Base.mkContext cfg
    evalStateT s Z3State { uniqRef   = uref
                         , smbSupply = smbGen --sref
                         , context   = ctx
                         , consts    = Map.empty
                         }
  where smbGen :: [String]
        smbGen =  [ [c] | c <- ['a' .. 'z'] ] 
               ++ [ c:(show i) | i <- [0 :: Integer ..]
                               , c <- ['a' .. 'z']
                               ]

-- | New Uniq
uniq :: Z3 Uniq
uniq = do
    st <- return . uniqRef =<< get
    modifyZ3Ref st (\(Uniq i) -> Uniq $ i + 1)

-- | Fresh string
fresh :: Z3 String
fresh =
    do st <- get
       put st { smbSupply = tail (smbSupply st) }
       return $ head $ smbSupply st

-- | Lifted modifyIORef
modifyZ3Ref :: IORef a -> (a -> a) -> Z3 a
modifyZ3Ref r f = Z3 $ lift (readIORef r) >>= \a -> do
    lift $ modifyIORef r f
    return a

-- | Add a constant of type AST a to the state.
addConst :: (Z3Type a) => Base.AST a -> Z3 Uniq
addConst expr = do
    st <- get
    u  <- uniq
    put st { consts = Map.insert u (Exp expr) (consts st) }
    return u

-- * Constructing expressions

-- | Declare constants
decl :: forall a.(Z3Type a) => Z3 (Expr a)
decl = do
    ctx <- gets context
    str <- fresh
    smb <- mkStringSymbol_ ctx str
    (srt :: Base.Sort a) <- mkSort_ ctx
    cst <- mkConst_ ctx smb srt
    u   <- addConst cst
    return $ Const u

-- | Lifted Base functions
mkStringSymbol_ :: Base.Context -> String -> Z3 Base.Symbol
mkStringSymbol_ ctx = Z3 . lift . Base.mkStringSymbol ctx

mkConst_ :: Base.Context -> Base.Symbol -> Base.Sort a -> Z3 (Base.AST a)
mkConst_ ctx smb = Z3 . lift . Base.mkConst ctx smb

mkSort_ :: forall a. Z3Type a => Base.Context -> Z3 (Base.Sort a) 
mkSort_
    | SBool <- sortZ3 (TY :: TY a) = Z3 . lift . Base.mkBoolSort
    | SInt  <- sortZ3 (TY :: TY a) = Z3 . lift . Base.mkIntSort
    | SReal <- sortZ3 (TY :: TY a) = Z3 . lift . Base.mkRealSort
    | otherwise                    = error pANIC_SORTS 

-- | Error Messages
pANIC_SORTS :: String
pANIC_SORTS = "Panic! Impossible! A type of class Z3Type is not\
              \instance of the corresponding subclass (Z3Int, \
              \Z3Real, ...)!"

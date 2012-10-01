{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Module    : Z3.Lang.Bool
-- Copyright : (c) Iago Abal, 2012
--             (c) David Castro, 2012
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>,
--             David Castro <david.castro.dcp@gmail.com>
-- Stability : experimental

module Z3.Lang.Bool () where

import Control.Monad.State

import Z3.Base ( AST )
import Z3.Lang.Monad

type instance TypeZ3 Bool = Bool

instance IsTy Bool where
  compile = compileBool

instance IsScalar Bool where
  fromZ3Type = id
  toZ3Type = id

compileBool :: Expr Bool -> Z3 (AST Bool)
compileBool (Lit a)
    = flip mkLiteral_ (toZ3Type a) =<< gets context
compileBool (Const u)
    = getConst u
compileBool (Not b)
    = do ctx <- gets context
         b'  <- compileBool b
         mkNot_ ctx b'
compileBool (BoolBin op e1 e2)
    = do ctx <- gets context
         e1' <- compileBool e1
         e2' <- compileBool e2
         mkBoolBin_ ctx op e1' e2'
compileBool (BoolMulti op es)
    = do ctx <- gets context
         es' <- mapM compileBool es
         mkBoolMulti_ ctx op es'
compileBool (Neg e)
    = do ctx <- gets context
         e'  <- compileBool e
         mkUnaryMinus_ ctx e'
compileBool (CmpE op e1 e2)
    = do ctx <- gets context
         e1' <- compile e1
         e2' <- compile e2
         mkEq_ ctx op e1' e2'
compileBool (CmpI op e1 e2)
    = do ctx <- gets context
         e1' <- compile e1
         e2' <- compile e2
         mkCmp_ ctx op e1' e2'
compileBool (Ite b e1 e2)
    = do ctx <- gets context
         b'  <- compileBool b
         e1' <- compileBool e1
         e2' <- compileBool e2
         mkIte_ ctx b' e1' e2'
compileBool _
    = error "Z3.Lang.Bool.compileBool: Panic!\
        \ Impossible constructor in pattern matching!"

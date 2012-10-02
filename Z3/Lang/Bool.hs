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
    = mkLiteral (toZ3Type a)
compileBool (Const u)
    = getConst u
compileBool (Not b)
    = do b'  <- compileBool b
         mkNot b'
compileBool (BoolBin op e1 e2)
    = do e1' <- compileBool e1
         e2' <- compileBool e2
         mkBoolBin op e1' e2'
compileBool (BoolMulti op es)
    = do es' <- mapM compileBool es
         mkBoolMulti op es'
compileBool (Neg e)
    = do e'  <- compileBool e
         mkUnaryMinus e'
compileBool (CmpE op e1 e2)
    = do e1' <- compile e1
         e2' <- compile e2
         mkEq op e1' e2'
compileBool (CmpI op e1 e2)
    = do e1' <- compile e1
         e2' <- compile e2
         mkCmp op e1' e2'
compileBool (Ite b e1 e2)
    = do b'  <- compileBool b
         e1' <- compileBool e1
         e2' <- compileBool e2
         mkIte b' e1' e2'
compileBool _
    = error "Z3.Lang.Bool.compileBool: Panic!\
        \ Impossible constructor in pattern matching!"

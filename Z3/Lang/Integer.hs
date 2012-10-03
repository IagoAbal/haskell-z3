{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Module    : Z3.Lang.Integer
-- Copyright : (c) Iago Abal, 2012
--             (c) David Castro, 2012
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>,
--             David Castro <david.castro.dcp@gmail.com>
-- Stability : experimental

module Z3.Lang.Integer () where

import Z3.Base ( AST )
import Z3.Lang.Bool ()
import Z3.Lang.Exprs
import Z3.Lang.Monad

type instance TypeZ3 Integer = Integer

instance IsTy Integer where
  compile = compileInteger

instance IsScalar Integer where
  fromZ3Type = id
  toZ3Type = id

instance IsNum Integer where
instance IsInt Integer where

compileInteger :: Expr Integer -> Z3 (AST Integer)
compileInteger (Lit a)
  = mkLiteral (toZ3Type a)
compileInteger (Const u)
  = getConst u
compileInteger (Neg e)
  = mkUnaryMinus =<< compileInteger e
compileInteger (CRingArith op es)
  = mkCRingArith op =<< mapM compileInteger es
compileInteger (IntArith op e1 e2)
  = do e1' <- compileInteger e1
       e2' <- compileInteger e2
       mkIntArith op e1' e2'
compileInteger (Ite eb e1 e2)
  = do eb' <- compile eb
       e1' <- compileInteger e1
       e2' <- compileInteger e2
       mkIte eb' e1' e2'
compileInteger _
    = error "Z3.Lang.Integer.compileInteger: Panic!\
        \ Impossible constructor in pattern matching!"

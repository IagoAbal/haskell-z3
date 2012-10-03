{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |
-- Module    : Z3.Lang.Rational
-- Copyright : (c) Iago Abal, 2012
--             (c) David Castro, 2012
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>,
--             David Castro <david.castro.dcp@gmail.com>
-- Stability : experimental

module Z3.Lang.Rational () where

import Z3.Base ( AST )
import Z3.Lang.Bool ()
import Z3.Lang.Exprs
import Z3.Lang.Monad

type instance TypeZ3 Rational = Rational

instance IsTy Rational where
  compile = compileRational

instance IsScalar Rational where
  fromZ3Type = id
  toZ3Type = id

instance IsNum Rational where
instance IsReal Rational where

compileRational :: Expr Rational -> Z3 (AST Rational)
compileRational (Lit a)
  = mkLiteral (toZ3Type a)
compileRational (Const u)
  = getConst u
compileRational (Neg e)
  = mkUnaryMinus =<< compileRational e
compileRational (CRingArith op es)
  = mkCRingArith op =<< mapM compileRational es
compileRational (RealArith op e1 e2)
  = do e1' <- compileRational e1
       e2' <- compileRational e2
       mkRealArith op e1' e2'
compileRational (Ite eb e1 e2)
  = do eb' <- compile eb
       e1' <- compileRational e1
       e2' <- compileRational e2
       mkIte eb' e1' e2'
compileRational _
    = error "Z3.Lang.Rational.compileRational: Panic!\
        \ Impossible constructor in pattern matching!"

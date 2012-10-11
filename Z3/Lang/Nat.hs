{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module    : Z3.Lang.Nat
-- Copyright : (c) Iago Abal, 2012
--             (c) David Castro, 2012
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>,
--             David Castro <david.castro.dcp@gmail.com>

module Z3.Lang.Nat
  ( Nat )
  where

import Z3.Base ( AST )
import Z3.Lang.Exprs
import Z3.Lang.Monad
import Z3.Lang.Prelude

import Data.Typeable

newtype Nat = Nat { unNat :: Integer }
  deriving (Eq,Ord,Enum,Real,Typeable)

instance Show Nat where
  show = show . unNat

instance Num Nat where
  Nat n + Nat m = Nat $ n + m
  Nat n - Nat m
    | n >= m    = Nat $ n-m
    | otherwise = error "Cannot subtract"
  Nat n * Nat m = Nat $ n * m
  abs = id
  signum = Nat . signum . unNat
  negate 0 = 0
  negate _ = error "Cannot negate a natural number"
  fromInteger n | n >= 0    = Nat n
                | otherwise = error "Not a natural number"

instance Integral Nat where
  quotRem (Nat n) (Nat m) = (Nat q,Nat r)
    where (q,r) = quotRem n m
  toInteger = unNat

type instance TypeZ3 Nat = Integer

instance IsTy Nat where
  typeInv e = e >=* 0
  compile = compileNat
  fromZ3Type = Nat
  toZ3Type = unNat

instance IsNum Nat where
instance IsInt Nat where

compileNat :: Expr Nat -> Z3 (AST Integer)
compileNat (Lit a)
  = mkLiteral (toZ3Type a)
compileNat (Const _ u)
  = return u
compileNat (Neg e) = do
  assert $ e ==* 0
  mkUnaryMinus =<< compileNat e
compileNat (CRingArith Sub (e1:e2:es)) = compileNatSub e1 e2 es
compileNat (CRingArith op es)
  = mkCRingArith op =<< mapM compileNat es
compileNat (IntArith Quot e1 e2)
  = do aux <- let_ e2
       assert $ aux /=* 0
       e1' <- compileNat e1
       aux' <- compileNat aux
       mkIntArith Quot e1' aux' 
compileNat (IntArith op e1 e2)
  = do e1' <- compileNat e1
       e2' <- compileNat e2
       mkIntArith op e1' e2'
compileNat (Ite eb e1 e2)
  = do eb' <- compile eb
       e1' <- compileNat e1
       e2' <- compileNat e2
       mkIte eb' e1' e2'
compileNat (App _)
  = error "Function application not supported for Naturals"
compileNat _
    = error "Z3.Lang.Nat.compileNat: Panic!\
        \ Impossible constructor in pattern matching!"

compileNatSub :: Expr Nat -> Expr Nat -> [Expr Nat] -> Z3 (AST Integer)
compileNatSub e1 e2 es = do
  assert $ e1 >=* e2
  case es of
      []       -> mkCRingArith Sub =<< mapM compileNat [e1,e2]
      (e':es') -> do aux <- let_ (e1 - e2)
                     compileNatSub aux e' es'

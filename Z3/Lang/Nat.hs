{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

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

import Z3.Monad hiding ( Z3, mkEq, Pattern, evalZ3, evalZ3With )
import Z3.Lang.Exprs
import Z3.Lang.Monad
import Z3.Lang.Prelude
import Z3.Lang.TY

import Control.Applicative ( (<$>) )
import Data.Typeable

-- | This type allows to reason about natural numbers.
--
-- Naturals are just integers plus a @(>= 0)@ invariant, and we ensure that
-- this invariant is always preserved by transparently adding new assertions
-- to the context.
--
-- Note that arithmetic on naturals must result in natural numbers, otherwise
-- the problem becomes /unsatisfiable/.
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

instance Compilable (Expr Nat) where
  compile = compileNat

instance IsTy Nat where
  typeInv e = e >=* 0
  tc = tcNat

  mkSort    _ = mkIntSort
  mkLiteral   = mkInt . unNat
  getValue  v = Nat <$> getInt v


instance IsNum Nat where
instance IsInt Nat where

tcNat :: Expr Nat -> TCM ()
tcNat (Lit _) = ok
tcNat (Const _ _) = ok
tcNat (Tag _) = ok
tcNat (Neg e) = do
  newTCC [e ==* 0]
  tcNat e
tcNat (CRingArith Sub es) = do
  newTCC $ zipWith (>=*) (scanl1 (-) es) (tail es)
  mapM_ tcNat es
tcNat (CRingArith _op es) = mapM_ tcNat es
tcNat (IntArith _op e1 e2) = do
  newTCC [e2 /=* 0]
  tcNat e1
  tcNat e2
tcNat (Ite eb e1 e2) = do
  tc eb
  withHypo eb $ tcNat e1
  withHypo (Not eb) $ tcNat e2
tcNat (App _) = ok
tcNat (Cast e) = tc e
tcNat _ = error "Z3.Lang.Nat.tcNat: Panic!\
            \ Impossible constructor in pattern matching!"

compileNat :: Expr Nat -> Z3 AST
compileNat (Lit a)
  = mkLiteral (unNat a)
compileNat (Const _ u)
  = return u
compileNat (Tag lyt)
  = do ix <- deBruijnIx lyt
       srt <- mkSort (TY :: TY Nat)
       mkBound ix srt
compileNat (Neg e)
  = mkUnaryMinus =<< compileNat e
compileNat (CRingArith op es)
  = mkCRingArith op =<< mapM compileNat es
compileNat (IntArith op e1 e2)
  = do e1' <- compileNat e1
       e2' <- compileNat e2
       mkIntArith op e1' e2'
compileNat (Ite eb e1 e2)
  = do eb' <- compile eb
       e1' <- compileNat e1
       e2' <- compileNat e2
       mkIte eb' e1' e2'
compileNat (App e)
    = compile e
compileNat (Cast (e :: Expr a))
    = compile e >>= compileCast (TY :: TY (a, Nat))
compileNat _
    = error "Z3.Lang.Nat.compileNat: Panic!\
        \ Impossible constructor in pattern matching!"

instance Castable Nat Integer where
  compileCast _ = return


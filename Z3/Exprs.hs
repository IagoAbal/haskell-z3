{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module    : Z3.Exprs
-- Copyright : (c) Iago Abal, 2012
--             (c) David Castro, 2012
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>,
--             David Castro <david.castro.dcp@gmail.com>
-- Stability : experimental

-- TODO: Pretty-printing of expressions

module Z3.Exprs (

    module Z3.Types
    
    -- * Abstract syntax
    , Expr

    -- * Constructing expressions
    , true
    , false
    , not_
    , and_, (&&*)
    , or_, (||*)
    , xor
    , implies, (==>)
    , iff, (<=>)
    , (//), (%*), (%%)
    , (==*), (/=*)
    , (<=*), (<*)
    , (>=*), (>*) 
    , ite

    ) where


import Z3.Exprs.Internal
import Z3.Types

import Data.Typeable ( Typeable1(..), typeOf )
import Unsafe.Coerce ( unsafeCoerce )


deriving instance Show (Expr a)
deriving instance Typeable1 Expr

instance Eq (Expr a) where
  (Lit l1) == (Lit l2) = l1 == l2
  (Const a) == (Const b) = a == b
  (Not e1) == (Not e2) = e1 == e2
  (BoolBin op1 p1 q1) == (BoolBin op2 p2 q2)
    = op1 == op2 && p1 == p2 && q1 == q2
  (BoolMulti op1 ps) == (BoolMulti op2 qs)
    | length ps == length qs = op1 == op2 && and (zipWith (==) ps qs)
  (Neg e1) == (Neg e2) = e1 == e2
  (CRingArith op1 as) == (CRingArith op2 bs)
    | length as == length bs = op1 == op2 && and (zipWith (==) as bs)
  (IntArith op1 a1 b1) == (IntArith op2 a2 b2)
    = op1 == op2 && a1 == a2 && b1 == b2
  (RealArith op1 a1 b1) == (RealArith op2 a2 b2)
    = op1 == op2 && a1 == a2 && b1 == b2
  (CmpE op1 a1 b1) == (CmpE op2 a2 b2)
    | op1 == op2 && typeOf a1 == typeOf a2
    = a1 == unsafeCoerce a2 && b1 == unsafeCoerce b2
  (CmpI op1 a1 b1) == (CmpI op2 a2 b2)
    | op1 == op2 && typeOf a1 == typeOf a2
    = a1 == unsafeCoerce a2 && b1 == unsafeCoerce b2
  (Ite g1 a1 b1) == (Ite g2 a2 b2) = g1 == g2 && a1 == a2 && b1 == b2
  _e1 == _e2 = False


-- * Constructing expressions

instance IsNum a => Num (Expr a) where
  (CRingArith Add as) + (CRingArith Add bs) = CRingArith Add (as ++ bs)
  (CRingArith Add as) + b = CRingArith Add (b:as)
  a + (CRingArith Add bs) = CRingArith Add (a:bs)
  a + b = CRingArith Add [a,b]
  (CRingArith Mul as) * (CRingArith Mul bs) = CRingArith Mul (as ++ bs)
  (CRingArith Mul as) * b = CRingArith Mul (b:as)
  a * (CRingArith Mul bs) = CRingArith Mul (a:bs)
  a * b = CRingArith Mul [a,b]
  (CRingArith Sub as) - b = CRingArith Sub (as ++ [b])
  a - b = CRingArith Sub [a,b]
  negate = Neg
  abs e = ite (e >=* 0) e (-e)
  signum e = ite (e >* 0) 1 (ite (e ==* 0) 0 (-1))
  fromInteger = literal . fromInteger

instance IsReal a => Fractional (Expr a) where
  (/) = RealArith Div
  fromRational = literal . fromRational

infixl 7  //, %*, %%
infix  4  ==*, /=*, <*, <=*, >=*, >*
infixr 3  &&*, ||*, `xor`
infixr 2  `implies`, `iff`, ==>, <=>

-- | /literal/ constructor.
--
literal :: IsScalar a => a -> Expr a
literal = Lit

-- | Boolean literals.
--
true, false :: Expr Bool
true  = Lit True
false = Lit False

-- | Boolean negation
--
not_ :: Expr Bool -> Expr Bool
not_ = Not

-- | Boolean binary /xor/.
--
xor :: Expr Bool -> Expr Bool -> Expr Bool
xor = BoolBin Xor
-- | Boolean implication
--
implies :: Expr Bool -> Expr Bool -> Expr Bool
implies = BoolBin Implies
-- | An alias for 'implies'.
--
(==>) :: Expr Bool -> Expr Bool -> Expr Bool
(==>) = implies
-- | Boolean /if and only if/.
--
iff :: Expr Bool -> Expr Bool -> Expr Bool
iff = BoolBin Iff
-- | An alias for 'iff'.
--
(<=>) :: Expr Bool -> Expr Bool -> Expr Bool
(<=>) = iff

-- | Boolean variadic /and/.
--
and_ :: [Expr Bool] -> Expr Bool
and_ = BoolMulti And
-- | Boolean variadic /or/.
--
or_ :: [Expr Bool] -> Expr Bool
or_  = BoolMulti Or

-- | Boolean binary /and/.
--
(&&*) :: Expr Bool -> Expr Bool -> Expr Bool
(BoolMulti And ps) &&* (BoolMulti And qs) = and_ (ps ++ qs)
(BoolMulti And ps) &&* q = and_ (q:ps)
p &&* (BoolMulti And qs) = and_ (p:qs)
p &&* q = and_ [p,q]
-- | Boolean binary /or/.
--
(||*) :: Expr Bool -> Expr Bool -> Expr Bool
(BoolMulti Or ps) ||* (BoolMulti Or qs) = or_ (ps ++ qs)
(BoolMulti Or ps) ||* q = or_ (q:ps)
p ||* (BoolMulti Or qs) = or_ (p:qs)
p ||* q = or_ [p,q]

-- | Integer division.
--
(//) :: IsInt a => Expr a -> Expr a -> Expr a
(//) = IntArith Quot
-- | Integer modulo.
--
(%*) :: IsInt a => Expr a -> Expr a -> Expr a
(%*) = IntArith Mod
-- | Integer remainder.
--
(%%) :: IsInt a => Expr a -> Expr a -> Expr a
(%%) = IntArith Rem


-- | Equals.
--
(==*) :: IsScalar a => Expr a -> Expr a -> Expr Bool
(==*) = CmpE Eq
-- | Not equals.
--
(/=*) :: IsScalar a => Expr a -> Expr a -> Expr Bool
(/=*) = CmpE Neq


-- | Less or equals than.
--
(<=*) :: IsNum a => Expr a -> Expr a -> Expr Bool
(<=*) = CmpI Le
-- | Less than.
--
(<*) :: IsNum a => Expr a -> Expr a -> Expr Bool
(<*) = CmpI Lt
-- | Greater or equals than.
--
(>=*) :: IsNum a => Expr a -> Expr a -> Expr Bool
(>=*) = CmpI Ge
-- | Greater than.
--
(>*) :: IsNum a => Expr a -> Expr a -> Expr Bool
(>*) = CmpI Gt

-- | /if-then-else/.
--
ite :: IsTy a => Expr Bool -> Expr a -> Expr a -> Expr a
ite = Ite

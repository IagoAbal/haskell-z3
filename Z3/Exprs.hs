{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module    : Z3.Exprs
-- Copyright : (c) Iago Abal, 2011 
--             (c) David Castro, 2011
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>, 
--             David Castro <david.castro.dcp@gmail.com>

module Z3.Exprs where


import Z3.Types

import Unsafe.Coerce ( unsafeCoerce )



-- * Abstract syntax

type Uniq = Int


data Expr :: * -> * where
  -- | Literals
  Lit :: (Z3Scalar a) => a -> Expr a
  -- | Constants
  Const :: !Uniq -> Expr a
  -- | Logical negation
  Not :: Expr Bool -> Expr Bool
  -- | Binary boolean expressions
  BoolBin :: BoolBinOp -> Expr Bool -> Expr Bool -> Expr Bool
  -- | Variadic boolean expressions
  BoolMulti :: BoolMultiOp -> [Expr Bool] -> Expr Bool
  -- | Arithmetic negation
  Neg :: Z3Num a => Expr a -> Expr a
  -- | Arithmetic expressions for commutative rings
  CRingArith :: Z3Num a => CRingOp -> Expr a -> Expr a -> Expr a
  -- | Integer arithmetic
  IntArith :: Z3Int a => IntOp -> Expr a -> Expr a -> Expr a
  -- | Real arithmetic
  RealArith :: Z3Real a => RealOp -> Expr a -> Expr a -> Expr a
  -- | Comparison expressions
  Cmp :: Z3Type a => CmpOp -> Expr a -> Expr a -> Expr Bool
  -- | if-then-else expressions
  Ite :: Z3Type a => Expr Bool -> Expr a -> Expr a -> Expr a

data BoolBinOp = Xor | Implies | Iff
    deriving (Eq,Show)

data BoolMultiOp = And | Or
    deriving (Eq,Show)

data CRingOp = Add | Mul | Sub
    deriving (Eq,Show)

data IntOp = Quot | Mod | Rem
    deriving (Eq,Show)

data RealOp = Div
    deriving (Eq,Show)

data CmpOp = Eq | Neq | Le | Lt | Ge | Gt
    deriving (Eq,Show)


-- * Constructing expressions

true, false :: Expr Bool
true = Lit True
false = Lit False

not_ :: Expr Bool -> Expr Bool
not_ = Not

-- TODO: infixr declarations for ==> and <=>

xor, implies, (==>), iff, (<=>) :: Expr Bool -> Expr Bool -> Expr Bool
xor = BoolBin Xor
implies = BoolBin Implies
(==>) = implies
iff = BoolBin Iff
(<=>) = iff

and_, or_ :: [Expr Bool] -> Expr Bool
and_ = BoolMulti And
or_ = BoolMulti Or

-- TODO: infixl declarations for &&* and ||*

(&&*), (||*) :: Expr Bool -> Expr Bool -> Expr Bool
p &&* q = and_ [p,q]
p ||* q = or_ [p,q]

quot_, mod_, rem_ :: Z3Int a => Expr a -> Expr a -> Expr a
quot_ = IntArith Quot
mod_ = IntArith Mod
rem_ = IntArith Rem

(./.), (.%.) :: Z3Int a => Expr a -> Expr a -> Expr a
(./.) = quot_
(.%.) = mod_

-- TODO: infix declarations

(==*), (/=*), (<=*), (<*), (>=*), (>*) :: Z3Type a => Expr a -> Expr a -> Expr Bool
(==*) = Cmp Eq
(/=*) = Cmp Neq
(<=*) = Cmp Le
(<*) = Cmp Lt
(>=*) = Cmp Ge
(>*) = Cmp Gt

ite :: Z3Type a => Expr Bool -> Expr a -> Expr a -> Expr a
ite = Ite


instance Eq (Expr a) where
  (Lit l1) == (Lit l2) = l1 == l2
  (Const a) == (Const b) = a == b
  (Not e1) == (Not e2) = e1 == e2
  (BoolBin op1 p1 q1) == (BoolBin op2 p2 q2)
    = op1 == op2 && p1 == p2 && q1 == q2
  (BoolMulti op1 ps) == (BoolMulti op2 qs)
    | length ps == length qs = op1 == op2 && and (zipWith (==) ps qs)
  (Neg e1) == (Neg e2) = e1 == e2
  (CRingArith op1 a1 b1) == (CRingArith op2 a2 b2)
    = op1 == op2 && a1 == a2 && b1 == b2
  (IntArith op1 a1 b1) == (IntArith op2 a2 b2)
    = op1 == op2 && a1 == a2 && b1 == b2
  (RealArith op1 a1 b1) == (RealArith op2 a2 b2)
    = op1 == op2 && a1 == a2 && b1 == b2
  (Cmp op1 a1 b1) == (Cmp op2 a2 b2)
    = op1 == op2 && a1 == (unsafeCoerce a2) && b1 == (unsafeCoerce b2)
  (Ite g1 a1 b1) == (Ite g2 a2 b2) = g1 == g2 && a1 == a2 && b1 == b2
  _e1 == _e2 = False

  -- TODO Pretty-printing
deriving instance Show (Expr a)

literal :: (Z3Scalar a) => a -> Expr a
literal = Lit

instance Z3Num a => Num (Expr a) where
  (+) = CRingArith Add
  (*) = CRingArith Mul
  (-) = CRingArith Sub
  negate = Neg
  abs e = ite (e >=* 0) e (-e)
  signum e = ite (e >* 0) 1 (ite (e ==* 0) 0 (-1))
  fromInteger = literal . fromInteger

instance Z3Real a => Fractional (Expr a) where
  (/) = RealArith Div
  fromRational = literal . fromRational

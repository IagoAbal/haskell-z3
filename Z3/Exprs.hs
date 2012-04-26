{-# LANGUAGE DeriveDataTypeable #-}
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


import Z3.Types

import Data.Typeable ( Typeable1(..), typeOf )
import Unsafe.Coerce ( unsafeCoerce )



-- * Abstract syntax

type Uniq = Int


data Expr :: * -> * where
  -- | Literals
  Lit :: Z3Scalar a => a -> Expr a
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
  CRingArith :: Z3Num a => CRingOp -> [Expr a] -> Expr a
  -- | Integer arithmetic
  IntArith :: Z3Int a => IntOp -> Expr a -> Expr a -> Expr a
  -- | Real arithmetic
  RealArith :: Z3Real a => RealOp -> Expr a -> Expr a -> Expr a
  -- | Comparison expressions
  Cmp :: Z3Type a => CmpOp -> Expr a -> Expr a -> Expr Bool
  -- | if-then-else expressions
  Ite :: Z3Type a => Expr Bool -> Expr a -> Expr a -> Expr a

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
  (Cmp op1 a1 b1) == (Cmp op2 a2 b2)
    | typeOf a1 == typeOf a2
    = op1 == op2 && a1 == (unsafeCoerce a2) && b1 == (unsafeCoerce b2)
  (Ite g1 a1 b1) == (Ite g2 a2 b2) = g1 == g2 && a1 == a2 && b1 == b2
  _e1 == _e2 = False


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

infixl 7  //, %*, %%
infix  4  ==*, /=*, <*, <=*, >=*, >*
infixr 3  &&*, ||*, `xor`
infixr 2  `implies`, `iff`, ==>, <=>

literal :: Z3Scalar a => a -> Expr a
literal = Lit

true, false :: Expr Bool
true = Lit True
false = Lit False

not_ :: Expr Bool -> Expr Bool
not_ = Not

xor, implies, (==>), iff, (<=>) :: Expr Bool -> Expr Bool -> Expr Bool
xor = BoolBin Xor
implies = BoolBin Implies
(==>) = implies
iff = BoolBin Iff
(<=>) = iff

and_, or_ :: [Expr Bool] -> Expr Bool
and_ = BoolMulti And
or_ = BoolMulti Or

(&&*), (||*) :: Expr Bool -> Expr Bool -> Expr Bool
(BoolMulti And ps) &&* (BoolMulti And qs) = and_ (ps ++ qs)
(BoolMulti And ps) &&* q = and_ (q:ps)
p &&* (BoolMulti And qs) = and_ (p:qs)
p &&* q = and_ [p,q]
(BoolMulti Or ps) ||* (BoolMulti Or qs) = or_ (ps ++ qs)
(BoolMulti Or ps) ||* q = or_ (q:ps)
p ||* (BoolMulti Or qs) = or_ (p:qs)
p ||* q = or_ [p,q]

(//), (%*), (%%) :: Z3Int a => Expr a -> Expr a -> Expr a
-- | Integer division
(//) = IntArith Quot
-- | Modulo
(%*) = IntArith Mod
-- | Remainder
(%%) = IntArith Rem

(==*), (/=*), (<=*), (<*), (>=*), (>*) :: Z3Type a => Expr a -> Expr a -> Expr Bool
(==*) = Cmp Eq
(/=*) = Cmp Neq
(<=*) = Cmp Le
(<*) = Cmp Lt
(>=*) = Cmp Ge
(>*) = Cmp Gt

-- | if-then-else
ite :: Z3Type a => Expr Bool -> Expr a -> Expr a -> Expr a
ite = Ite


instance Z3Num a => Num (Expr a) where
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
  negate e = Neg e
  abs e = ite (e >=* 0) e (-e)
  signum e = ite (e >* 0) 1 (ite (e ==* 0) 0 (-1))
  fromInteger = literal . fromInteger

instance Z3Real a => Fractional (Expr a) where
  (/) = RealArith Div
  fromRational = literal . fromRational

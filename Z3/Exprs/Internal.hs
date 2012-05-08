
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module    : Z3.Exprs.Internal
-- Copyright : (c) Iago Abal, 2011 
--             (c) David Castro, 2011
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>, 
--             David Castro <david.castro.dcp@gmail.com>

module Z3.Exprs.Internal where


import Z3.Types

import Data.Typeable ( Typeable1 )


-- | Unique identifiers
newtype Uniq = Uniq Int deriving (Eq, Ord, Show)

{-# WARNING Lit
          , Const
          , Not
          , BoolBin
          , BoolMulti
          , Neg
          , CRingArith
          , IntArith
          , RealArith
          , Cmp
          , Ite
          "You are using a constructor of type Expr, \
          \which you should NOT be using! \
          \In fact, you should not be importing this \
          \module at all! Import Z3.Exprs instead!" #-}
 
-- | Abstract syntax
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
  Cmp :: Z3Type a => CmpOp a -> Expr a -> Expr a -> Expr Bool
  -- | if-then-else expressions
  Ite :: Z3Type a => Expr Bool -> Expr a -> Expr a -> Expr a

{-# WARNING BoolBinOp
          , BoolMultiOp
          , CRingOp
          , IntOp
          , RealOp
          , CmpOp
          "You should NOT be using this type or data constructor! \
          \In fact, you should not be importing this \
          \module at all! Import Z3.Exprs instead!" #-}

-- | Boolean binary operations.
data BoolBinOp = Xor | Implies | Iff
    deriving (Eq,Show)

-- | Boolean polyadic operations.
data BoolMultiOp = And | Or
    deriving (Eq,Show)

-- | Commutative ring operations.
data CRingOp = Add | Mul | Sub
    deriving (Eq,Show)

-- | Operations for sort /int/.
data IntOp = Quot | Mod | Rem
    deriving (Eq,Show)

-- | Operations for sort /real/.
data RealOp = Div
    deriving (Eq,Show)

-- | Comparison operations.
data CmpOp :: * -> * where
  Eq  :: Z3Type a => CmpOp a
  Neq :: Z3Type a => CmpOp a
  Le  :: Z3Num a => CmpOp a
  Lt  :: Z3Num a => CmpOp a
  Ge  :: Z3Num a => CmpOp a
  Gt  :: Z3Num a => CmpOp a

deriving instance Eq (CmpOp a)
deriving instance Show (CmpOp a)
deriving instance Typeable1 CmpOp

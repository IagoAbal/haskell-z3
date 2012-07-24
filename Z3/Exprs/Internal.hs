
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

-- |
-- Module    : Z3.Exprs.Internal
-- Copyright : (c) Iago Abal, 2012
--             (c) David Castro, 2012
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>,
--             David Castro <david.castro.dcp@gmail.com>
-- Stability : experimental

module Z3.Exprs.Internal where


import Z3.Types

import Data.Typeable ( Typeable )


-- | Unique identifiers
--
type Uniq = Int

{-# WARNING Lit
          , Const
          , Not
          , BoolBin
          , BoolMulti
          , Neg
          , CRingArith
          , IntArith
          , RealArith
          , CmpE
          , CmpI
          , Ite
          "You are using a constructor of type Expr, \
          \which you should NOT be using! \
          \In fact, you should not be importing this \
          \module at all! Import Z3.Exprs instead!" #-}

-- | Abstract syntax.
--
data Expr :: * -> * where
  --  | Literals
  Lit :: IsScalar a => a -> Expr a
  --  | Constants
  Const :: !Uniq -> Expr a
  --  | Logical negation
  Not :: Expr Bool -> Expr Bool
  --  | Binary boolean expressions
  BoolBin :: BoolBinOp -> Expr Bool -> Expr Bool -> Expr Bool
  --  | Variadic boolean expressions
  BoolMulti :: BoolMultiOp -> [Expr Bool] -> Expr Bool
  --  | Arithmetic negation
  Neg :: IsNum a => Expr a -> Expr a
  --  | Arithmetic expressions for commutative rings
  CRingArith :: IsNum a => CRingOp -> [Expr a] -> Expr a
  --  | Integer arithmetic
  IntArith :: IsInt a => IntOp -> Expr a -> Expr a -> Expr a
  --  | Real arithmetic
  RealArith :: IsReal a => RealOp -> Expr a -> Expr a -> Expr a
  --  | Comparison expressions
  CmpE :: IsScalar a => CmpOpE -> Expr a -> Expr a -> Expr Bool
  CmpI :: IsNum a => CmpOpI -> Expr a -> Expr a -> Expr Bool
  --  | if-then-else expressions
  Ite :: IsTy a => Expr Bool -> Expr a -> Expr a -> Expr a

{-# WARNING BoolBinOp
          , BoolMultiOp
          , CRingOp
          , IntOp
          , RealOp
          , CmpOpE
          , CmpOpI
          "You should NOT be using this type or data constructor! \
          \In fact, you should not be importing this \
          \module at all! Import Z3.Exprs instead!" #-}

-- | Boolean binary operations.
data BoolBinOp = Xor | Implies | Iff
    deriving (Eq,Show)

-- | Boolean variadic operations.
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

-- | Equality testing.
data CmpOpE = Eq | Neq
    deriving (Eq, Show, Typeable)

-- | Inequality comparisons.
data CmpOpI = Le | Lt | Ge | Gt
    deriving (Eq, Show, Typeable)

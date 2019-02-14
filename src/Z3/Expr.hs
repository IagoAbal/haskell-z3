
-- |
-- Module    : Z3.Expr
-- Copyright : (c) Iago Abal, 2019--present
-- License   : BSD3
-- Maintainer: Iago Abal <mail@iagoabal.eu>
--
-- Pure combinators to build Z3 expressions.
-- For now, just covering Boolean, Integer, and Real expressions.

module Z3.Expr where

import qualified Z3.Base as Base

---------------------------------------------------------------------
-- AST

-- TODO: Bit-vectors, arrays, sets, etc.

data Type :: * where
  TySort  :: Base.Sort -> Type
  TyBool  :: Type
  TyInt   :: Type
  TyReal  :: Type

-- | Expressions.
data Expr :: * where
  -- | Arbitrary expressions
  Exp :: Base.AST -> Expr
  -- | Literals
  Lit :: Lit -> !Type -> Expr
  -- | Unary operations
  Una :: !UnOp -> Expr -> Expr
  -- | Binary operations
  Bin :: !BinOp -> Expr -> Expr -> Expr
  -- | Associative operations
  Assoc :: !AssocOp -> [Expr] -> Expr
  -- | Equality operations
  Equ :: !EquOp -> [Expr] -> Expr
  -- | If-then-else expressions
  Ite :: Expr -> Expr -> Expr -> Expr
  -- | Function applications
  App :: FunApp -> Expr
  --  | Tag, for converting from HOAS to de-Bruijn
  Tag :: !Layout -> Expr
  -- | Quantified formulas
  Quant :: QExpr qexpr => Quantifier -> qexpr -> Expr

-- | Quantifier layout level.
type Layout = Int

data Lit :: * where
  LInt  :: Integral int => int -> Lit
  LBool :: Bool -> Lit

data UnOp = Not | Neg

data BinOp =
  | Xor | Implies | Iff
  | Sub | Div | Mod | Rem
  | Le | Lt | Ge | Gt

data AssocOp =
  | And | Or
  | Add | Mult

data EquOp = Equal | Distinct

data FunApp :: * where
  --  | Function declaration
  FuncDecl :: Base.FuncDecl -> FunApp a
  --  | Partial application
  PApp :: FunApp (Expr -> r) -> Expr -> FunApp r

-- | Quantifiers
data Quantifier = ForAll | Exists
  deriving (Eq, Show)

-- | Quantifiable expressions.
class QExpr t where
  compileQuant :: Quantifier -> [Base.Symbol] -> [Base.Sort] -> t -> Z3 Base.AST

---------------------------------------------------------------------
-- Smart constructors

tySort :: Base.Sort -> Type
tySort = TySort

tyBool, tyInt, tyReal :: Type
tyBool = TyBool
tyInt  = TyInt
tyReal = TyReal

expr :: Base.AST -> Expr
expr = Exp

lit :: Integral int => int -> Type -> Expr
lit = Lit

var :: Base.AST -> Expr
var = expr

neg :: Expr -> Expr
neg = Una Neg

add :: [Expr] -> Expr
add = Assoc Add

sub :: Expr -> Expr -> Expr
sub = Bin Sub

eq :: [Expr] -> Expr
eq = Equ Equal

distinct :: [Expr] -> Expr
distinct = Equ Distinct

ite :: Expr -> Expr -> Expr -> Expr
ite = Ite

---------------------------------------------------------------------
-- Instances

instance Eq Expr where
  x == y = eq [x, y]
  x /= y = distinct [x, y]

instance Num Expr where
  fromInteger n = lit n tyInt
  negate x = neg x
  x + y = add [x, y]
  x - y = sub x y

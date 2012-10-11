{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module    : Z3.Lang.Prelude
-- Copyright : (c) Iago Abal, 2012
--             (c) David Castro, 2012
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>,
--             David Castro <david.castro.dcp@gmail.com>
-- Stability : experimental
--

-- TODO: Pretty-printing of expressions

module Z3.Lang.Prelude (

    -- * Z3 script
      Z3
    , Base.Result(..)
    , evalZ3

    -- ** Commands
    , var
    , fun1, fun2, fun3, fun4, fun5
    , assert
    , let_
    , check
    , checkModel

    -- * Expressions
    , Expr
    , IsTy
    , IsFun
    , IsNum
    , IsInt
    , IsReal
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

import Z3.Base ( AST )
import qualified Z3.Base as Base
import Z3.Lang.Exprs
import Z3.Lang.Monad

import Control.Applicative ( (<$>) )
import Control.Monad ( liftM, liftM2, liftM3, liftM4, liftM5, join )
#if __GLASGOW_HASKELL__ < 704
import Data.Typeable ( Typeable1(..), typeOf )
import Unsafe.Coerce ( unsafeCoerce )
#else
import Data.Typeable ( Typeable1(..) )
#endif

---------------------------------------------------------------------
-- Commands

-- | Declare skolem variables.
--
var :: forall a. IsTy a => Z3 (Expr a)
var = do
    (u, str) <- fresh
    smb <- mkStringSymbol str
    (srt :: Base.Sort (TypeZ3 a)) <- mkSort
    cnst <- mkConst smb srt
    let e = Const u cnst
    assert $ typeInv e
    return e

-- | Declare uninterpreted function of arity 1
--
fun1 :: (IsTy a, IsTy b, b ~ TypeZ3 b) => Z3 (Expr a -> Expr b)
fun1 = do
    (fd :: FunApp (a -> b)) <- fun 
    return $ \e -> App $ PApp fd e

-- | Declare uninterpreted function of arity 2
--
fun2 :: (IsTy a, IsTy b, IsTy c, c ~ TypeZ3 c) => Z3 (Expr a -> Expr b -> Expr c)
fun2 = do
    (fd :: FunApp (a -> b -> c)) <- fun 
    return $ \e1 e2 -> App $ PApp (PApp fd e1) e2


-- | Declare uninterpreted function of arity 3
--
fun3 :: (IsTy a, IsTy b, IsTy c, IsTy d, d ~ TypeZ3 d)
     => Z3 (Expr a -> Expr b -> Expr c -> Expr d)
fun3 = do
    (fd :: FunApp (a -> b -> c -> d)) <- fun 
    return $ \e1 e2 e3 ->
        App $ PApp (PApp (PApp fd e1) e2) e3

-- | Declare uninterpreted function of arity 4
--
fun4 :: (IsTy a, IsTy b, IsTy c, IsTy d, IsTy e, e ~ TypeZ3 e)
     => Z3 (Expr a -> Expr b -> Expr c -> Expr d -> Expr e)
fun4 = do
    (fd :: FunApp (a -> b -> c -> d -> e)) <- fun 
    return $ \e1 e2 e3 e4 ->
        App $ PApp (PApp (PApp (PApp fd e1) e2) e3) e4

-- | Declare uninterpreted function of arity 5
--
fun5 :: (IsTy a, IsTy b, IsTy c, IsTy d, IsTy e, IsTy f, f ~ TypeZ3 f)
     => Z3 (Expr a -> Expr b -> Expr c -> Expr d -> Expr e -> Expr f)
fun5 = do
    (fd :: FunApp (a -> b -> c -> d -> e -> f)) <- fun 
    return $ \e1 e2 e3 e4 e5 ->
        App $ PApp (PApp (PApp (PApp (PApp fd e1) e2) e3) e4) e5

-- | Declare uninterpreted function
--
fun :: forall a. IsFun a => Z3 (FunApp a)
fun = do
  (_u, str) <- fresh
  smb <- mkStringSymbol str
  (fd :: Base.FuncDecl (TypeZ3 a)) <- mkFuncDecl smb
  return (FuncDecl fd)

-- | Make assertion in current context.
--
assert :: Expr Bool -> Z3 ()
assert (Lit True) = return ()
assert e          = compile e >>= assertCnstr

-- | Introduce an auxiliary declaration to name a given expression.
--
-- If you really want sharing use this instead of Haskell's /let/.
--
let_ :: IsTy a => Expr a -> Z3 (Expr a)
let_ e@(Lit _)   = return e
let_ e@(Const _ _) = return e
let_ e = do
  aux <- var
  assert (aux ==* e)
  return aux

-- | Check expression in current model (checks current model if none exists)
--
checkModel :: forall a.IsTy a => Expr a -> Z3 (Result a)
checkModel e = do
  a <- compile e
  m <- getModel
  fixResult a m
  where fixResult :: Base.AST (TypeZ3 a) -> Result Base.Model -> Z3 (Result a)
        fixResult _ (Base.Undef)  = return Base.Undef
        fixResult _ (Base.Unsat)  = return Base.Unsat
        fixResult a (Base.Sat m)  = peek =<< eval m a

        peek :: Maybe (Base.AST (TypeZ3 a)) -> Z3 (Base.Result a)
        peek (Just a) = Base.Sat . fromZ3Type <$> getValue a
        peek Nothing  = error "Z3.Lang.Monad.eval: quantified expression or partial model!"

----------------------------------------------------------------------
-- Expressions

deriving instance Typeable1 Expr

-- In GHC 7.4 Eq and Show are no longer superclasses of Num
#if __GLASGOW_HASKELL__ < 704
deriving instance Show (FunApp a)
deriving instance Show (Expr a)

instance Eq (Expr a) where
  _e1 == _e2 = error "Z3.Lang.Expr: equality not supported"
#endif

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
literal :: IsTy a => a -> Expr a
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
(==*) :: IsTy a => Expr a -> Expr a -> Expr Bool
(==*) = CmpE Eq
-- | Not equals.
--
(/=*) :: IsTy a => Expr a -> Expr a -> Expr Bool
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

----------------------------------------------------------------------
-- Functions
type instance TypeZ3 (a -> b) = TypeZ3 a -> TypeZ3 b

instance (IsTy a, IsTy b) => IsFun (a -> b) where
instance (IsTy a, IsFun (b -> c)) => IsFun (a -> b -> c) where

----------------------------------------------------------------------
-- Booleans

type instance TypeZ3 Bool = Bool

instance IsTy Bool where
  typeInv = const true
  compile = compileBool
  fromZ3Type = id
  toZ3Type = id

compileBool :: Expr Bool -> Z3 (AST Bool)
compileBool (Lit a)
    = mkLiteral a
compileBool (Const _ u)
    = return u
compileBool (Not b)
    = do b'  <- compileBool b
         mkNot b'
compileBool (BoolBin op e1 e2)
    = do e1' <- compileBool e1
         e2' <- compileBool e2
         mkBoolBin op e1' e2'
compileBool (BoolMulti op es)
    = do es' <- mapM compileBool es
         mkBoolMulti op es'
compileBool (Neg e)
    = do e'  <- compileBool e
         mkUnaryMinus e'
compileBool (CmpE op e1 e2)
    = do e1' <- compile e1
         e2' <- compile e2
         mkEq op e1' e2'
compileBool (CmpI op e1 e2)
    = do e1' <- compile e1
         e2' <- compile e2
         mkCmp op e1' e2'
compileBool (Ite b e1 e2)
    = do b'  <- compileBool b
         e1' <- compileBool e1
         e2' <- compileBool e2
         mkIte b' e1' e2'
compileBool (App e)
    = app2AST e
compileBool _
    = error "Z3.Lang.Prelude.compileBool: Panic! Impossible constructor in pattern matching!"

----------------------------------------------------------------------
-- Integers

type instance TypeZ3 Integer = Integer

instance IsTy Integer where
  typeInv = const true
  compile = compileInteger
  fromZ3Type = id
  toZ3Type = id

instance IsNum Integer where
instance IsInt Integer where

compileInteger :: Expr Integer -> Z3 (AST Integer)
compileInteger (Lit a)
  = mkLiteral a
compileInteger (Const _ u)
  = return u
compileInteger (Neg e)
  = mkUnaryMinus =<< compileInteger e
compileInteger (CRingArith op es)
  = mkCRingArith op =<< mapM compileInteger es
compileInteger (IntArith Quot e1 e2)
  = do aux <- let_ e2
       assert $ aux /=* 0
       e1' <- compileInteger e1
       aux' <- compileInteger aux
       mkIntArith Quot e1' aux' 
compileInteger (IntArith op e1 e2)
  = do e1' <- compileInteger e1
       e2' <- compileInteger e2
       mkIntArith op e1' e2'
compileInteger (Ite eb e1 e2)
  = do eb' <- compile eb
       e1' <- compileInteger e1
       e2' <- compileInteger e2
       mkIte eb' e1' e2'
compileInteger (App e)
    = app2AST e
compileInteger _
    = error "Z3.Lang.Prelude.compileInteger: Panic! Impossible constructor in pattern matching!"

----------------------------------------------------------------------
-- Rationals

type instance TypeZ3 Rational = Rational

instance IsTy Rational where
  typeInv = const true
  compile = compileRational
  fromZ3Type = id
  toZ3Type = id

instance IsNum Rational where
instance IsReal Rational where

compileRational :: Expr Rational -> Z3 (AST Rational)
compileRational (Lit a)
  = mkLiteral a
compileRational (Const _ u)
  = return u
compileRational (Neg e)
  = mkUnaryMinus =<< compileRational e
compileRational (CRingArith op es)
  = mkCRingArith op =<< mapM compileRational es
compileRational (RealArith op@Div e1 e2)
  = do aux <- let_ e2
       assert $ aux /=* 0
       e1' <- compileRational e1
       aux' <- compileRational aux
       mkRealArith op e1' aux'
compileRational (Ite eb e1 e2)
  = do eb' <- compile eb
       e1' <- compileRational e1
       e2' <- compileRational e2
       mkIte eb' e1' e2'
compileRational (App e)
    = app2AST e
compileRational _
    = error "Z3.Lang.Prelude.compileRational: Panic! Impossible constructor in pattern matching!"

app2AST :: forall a. IsTy a => FunApp a -> Z3 (Base.AST (TypeZ3 a))
app2AST (PApp(FuncDecl fd)e1) = join $
  liftM  (mkApp1 fd) (compile e1)
app2AST (PApp(PApp(FuncDecl fd)e1)e2) = join $
  liftM2 (mkApp2 fd) (compile e1) (compile e2)
app2AST (PApp(PApp(PApp(FuncDecl fd)e1)e2)e3) = join $
  liftM3 (mkApp3 fd) (compile e1) (compile e2) (compile e3)
app2AST (PApp(PApp(PApp(PApp(FuncDecl fd)e1)e2)e3)e4) = join $
  liftM4 (mkApp4 fd) (compile e1) (compile e2) (compile e3) (compile e4)
app2AST (PApp(PApp(PApp(PApp(PApp(FuncDecl fd)e1)e2)e3)e4)e5) = join $
  liftM5 (mkApp5 fd) (compile e1) (compile e2) (compile e3) (compile e4) (compile e5)
app2AST _ = error "Z3.Lang.Prelude.app2AST: Panic! Impossible function application!"

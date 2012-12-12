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
    , Args(..)
    , stdArgs
    , evalZ3With

    -- ** Commands
    , var
    , fun1, fun2, fun3, fun4, fun5
    , assert
    , let_
    , check
    , checkModel

    -- * Expressions
    , Expr
    , Pattern(..)
    , IsTy
    , IsNum
    , IsInt
    , IsReal
    , literal
    , true
    , false
    , not_
    , and_, (&&*)
    , or_, (||*)
    , xor
    , implies, (==>)
    , iff, (<=>)
    , forall, forallP
    , (//), (%*), (%%)
    , divides
    , (==*), (/=*)
    , (<=*), (<*)
    , (>=*), (>*) 
    , ite

    ) where

import Z3.Base ( AST )
import qualified Z3.Base as Base
import Z3.Lang.Exprs
import Z3.Lang.Monad

import Control.Applicative ( (<$>), (<*>), pure )
import Control.Monad ( liftM, liftM2, liftM3, liftM4, liftM5, join )
import Data.Maybe ( maybeToList )
import qualified Data.Traversable as T
#if __GLASGOW_HASKELL__ < 704
import Data.Typeable ( Typeable1(..), typeOf )
import Unsafe.Coerce ( unsafeCoerce )
#else
import Data.Typeable ( Typeable1(..) )
#endif

---------------------------------------------------------------------
-- Utils

-- | Compile while introducing TCCs into the script.
--
compileWithTCC :: IsTy a => Expr a -> Z3 (Base.AST (TypeZ3 a))
compileWithTCC e = do
  assertCnstr =<< compile (and_ $ typecheck e)
  compile e

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

-- | Declare uninterpreted function of arity 1.
--
fun1 :: (IsTy a, IsTy b) => Z3 (Expr a -> Expr b)
fun1 = do
    (fd :: FunApp (a -> b)) <- funDecl
    let f e = App $ PApp fd e
    assert $ forall $ \a -> typeInv (f a)
    return f

-- | Declare uninterpreted function of arity 2.
--
fun2 :: (IsTy a, IsTy b, IsTy c) => Z3 (Expr a -> Expr b -> Expr c)
fun2 = do
    (fd :: FunApp (a -> b -> c)) <- funDecl 
    let f e1 e2 = App $ PApp (PApp fd e1) e2
    assert $ forall $ \a -> forall $ \b -> typeInv (f a b)
    return f


-- | Declare uninterpreted function of arity 3.
--
fun3 :: (IsTy a, IsTy b, IsTy c, IsTy d)
     => Z3 (Expr a -> Expr b -> Expr c -> Expr d)
fun3 = do
    (fd :: FunApp (a -> b -> c -> d)) <- funDecl
    let f e1 e2 e3 = App $ PApp (PApp (PApp fd e1) e2) e3
    assert $ forall $ \a -> 
             forall $ \b -> 
             forall $ \c -> 
               typeInv (f a b c)
    return f

-- | Declare uninterpreted function of arity 4.
--
fun4 :: (IsTy a, IsTy b, IsTy c, IsTy d, IsTy e)
     => Z3 (Expr a -> Expr b -> Expr c -> Expr d -> Expr e)
fun4 = do
    (fd :: FunApp (a -> b -> c -> d -> e)) <- funDecl
    let f e1 e2 e3 e4 = App $ PApp (PApp (PApp (PApp fd e1) e2) e3) e4
    assert $ forall $ \a -> 
             forall $ \b -> 
             forall $ \c -> 
             forall $ \d -> 
               typeInv (f a b c d)
    return f

-- | Declare uninterpreted function of arity 5.
--
fun5 :: (IsTy a, IsTy b, IsTy c, IsTy d, IsTy e, IsTy f)
     => Z3 (Expr a -> Expr b -> Expr c -> Expr d -> Expr e -> Expr f)
fun5 = do
    (fd :: FunApp (a -> b -> c -> d -> e -> f)) <- funDecl
    let f e1 e2 e3 e4 e5 = App $ PApp (PApp (PApp (PApp (PApp fd e1) e2) e3) e4) e5
    assert $ forall $ \a -> 
             forall $ \b -> 
             forall $ \c -> 
             forall $ \d -> 
             forall $ \e ->
               typeInv (f a b c d e)
    return f

-- | Declare uninterpreted function
--
funDecl :: forall a. IsFun a => Z3 (FunApp a)
funDecl = do
  (_u, str) <- fresh
  smb <- mkStringSymbol str
  (fd :: Base.FuncDecl (TypeZ3 a)) <- mkFuncDecl smb
  return (FuncDecl fd)

-- | Make assertion in current context.
--
assert :: Expr Bool -> Z3 ()
assert (Lit True) = return ()
assert e          = compileWithTCC e >>= assertCnstr

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

-- | Check satisfiability and evaluate the given expression if a model exists.
--
checkModel :: forall a. IsTy a => Expr a -> Z3 (Result a)
checkModel e = do
  a <- compileWithTCC e
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
p `implies` (BoolBin Implies q r)
  = (p &&* q) `implies` r
p `implies` q = BoolBin Implies p q
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
and_ [] = true
and_ bs = BoolMulti And bs
-- | Boolean variadic /or/.
--
or_ :: [Expr Bool] -> Expr Bool
or_ [] = false
or_ bs = BoolMulti Or bs

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

-- | Forall formula.
--
forall :: forall a. IsTy a => (Expr a -> Expr Bool) -> Expr Bool
forall f = ForAll f Nothing

forallP :: IsTy a => (Expr a -> Expr Bool)    -- ^ body
                      -> (Expr a -> Pattern)  -- ^ pattern
                      -> Expr Bool
forallP f = ForAll f . Just

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

-- | @k `divides` n == n %* k ==* 0@
--
divides :: IsInt a => Expr a -> Expr a -> Expr Bool
k `divides` n = n %* k ==* 0
{-# INLINE divides #-}

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
-- Booleans

type instance TypeZ3 Bool = Bool

instance Compilable (Expr Bool) where
  compile = compileBool

instance IsTy Bool where
  typeInv = const true
  tc = tcBool
  fromZ3Type = id
  toZ3Type = id

tcBool :: Expr Bool -> TCM ()
tcBool (Lit _)     = ok
tcBool (Const _ _) = ok
tcBool (Tag _) = ok
tcBool (Not b)     = tcBool b
tcBool (BoolBin Implies e1 e2) = do
  tcBool e1
  withHypo e1 $ tcBool e2
tcBool (BoolBin _op e1 e2) = do
  tcBool e1
  tcBool e2
tcBool (BoolMulti _op es) = mapM_ tcBool es
tcBool (ForAll _f _p) = ok
tcBool (CmpE _op e1 e2) = do
  tc e1
  tc e2
tcBool (CmpI _op e1 e2) = do
  tc e1
  tc e2
tcBool (Ite eb e1 e2) = do
  tcBool eb
  withHypo eb $ tcBool e1
  withHypo (Not eb) $ tcBool e2
tcBool (App _app) = ok
tcBool _
    = error "Z3.Lang.Prelude.tcBool: Panic! Impossible constructor in pattern matching!"

compileBool :: Expr Bool -> Z3 (AST Bool)
compileBool (Lit a)
    = mkLiteral a
compileBool (Const _ u)
    = return u
compileBool (Tag lyt)
    = do ix <- deBruijnIx lyt
         srt <- mkSort
         mkBound ix srt
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
compileBool (ForAll (f :: Expr a -> Expr Bool)
                    (mb_mk_pat :: Maybe (Expr a -> Pattern)))
    = do (_,n) <- fresh
         sx <- mkStringSymbol n
         srt :: Base.Sort (TypeZ3 a) <- mkSort
         (body,mb_pat) <- newQLayout $ \x -> liftM2 (,)
                              (compileBool $ mkBody x)
                              (T.mapM mkPat (mb_mk_pat <*> pure x))
         mkForall (maybeToList mb_pat) sx srt body
  where mkBody x = let b = f x in and_ (typeInv x:typecheck b) ==> b
        mkPat (Pat e) = do ast <- compile e; mkPattern [ast]
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
    = compile e
compileBool _
    = error "Z3.Lang.Prelude.compileBool: Panic! Impossible constructor in pattern matching!"

----------------------------------------------------------------------
-- Integers

type instance TypeZ3 Integer = Integer

instance Compilable (Expr Integer) where
  compile = compileInteger

instance IsTy Integer where
  typeInv = const true
  tc = tcInteger
  fromZ3Type = id
  toZ3Type = id

instance IsNum Integer where
instance IsInt Integer where

tcInteger :: Expr Integer -> TCM ()
tcInteger (Lit _) = ok
tcInteger (Const _ _) = ok
tcInteger (Tag _) = ok
tcInteger (Neg e) = tcInteger e
tcInteger (CRingArith _op es) = mapM_ tcInteger es
tcInteger (IntArith _op e1 e2) = do
  newTCC [e2 /=* 0]
  tcInteger e1
  tcInteger e2
tcInteger (Ite eb e1 e2) = do
  tc eb
  withHypo eb $ tcInteger e1
  withHypo (Not eb) $ tcInteger e2
tcInteger (App _) = ok
tcInteger _
    = error "Z3.Lang.Prelude.tcInteger: Panic! Impossible constructor in pattern matching!"

compileInteger :: Expr Integer -> Z3 (AST Integer)
compileInteger (Lit a)
  = mkLiteral a
compileInteger (Const _ u)
  = return u
compileInteger (Tag lyt)
  = do ix <- deBruijnIx lyt
       srt <- mkSort
       mkBound ix srt
compileInteger (Neg e)
  = mkUnaryMinus =<< compileInteger e
compileInteger (CRingArith op es)
  = mkCRingArith op =<< mapM compileInteger es
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
    = compile e
compileInteger _
    = error "Z3.Lang.Prelude.compileInteger: Panic! Impossible constructor in pattern matching!"

----------------------------------------------------------------------
-- Rationals

type instance TypeZ3 Rational = Rational

instance Compilable (Expr Rational) where
  compile = compileRational

instance IsTy Rational where
  typeInv = const true
  tc = tcRational
  fromZ3Type = id
  toZ3Type = id

instance IsNum Rational where
instance IsReal Rational where

tcRational :: Expr Rational -> TCM ()
tcRational (Lit _) = ok
tcRational (Const _ _) = ok
tcRational (Tag _) = ok
tcRational (Neg e) = tcRational e
tcRational (CRingArith _op es) = mapM_ tcRational es
tcRational (RealArith Div e1 e2) = do
  newTCC [e2 /=* 0]
  tcRational e1
  tcRational e2
tcRational (Ite eb e1 e2) = do
  tc eb
  withHypo eb $ tcRational e1
  withHypo (Not eb) $ tcRational e2
tcRational (App _) = ok
tcRational _
    = error "Z3.Lang.Prelude.tcRational: Panic! Impossible constructor in pattern matching!"

compileRational :: Expr Rational -> Z3 (AST Rational)
compileRational (Lit a)
  = mkLiteral a
compileRational (Const _ u)
  = return u
compileRational (Tag lyt)
  = do ix <- deBruijnIx lyt
       srt <- mkSort
       mkBound ix srt
compileRational (Neg e)
  = mkUnaryMinus =<< compileRational e
compileRational (CRingArith op es)
  = mkCRingArith op =<< mapM compileRational es
compileRational (RealArith op@Div e1 e2)
  = do e1' <- compileRational e1
       e2' <- compileRational e2
       mkRealArith op e1' e2'
compileRational (Ite eb e1 e2)
  = do eb' <- compile eb
       e1' <- compileRational e1
       e2' <- compileRational e2
       mkIte eb' e1' e2'
compileRational (App e)
    = compile e
compileRational _
    = error "Z3.Lang.Prelude.compileRational: Panic! Impossible constructor in pattern matching!"

----------------------------------------------------------------------
-- Functions

type instance TypeZ3 (a -> b) = TypeZ3 a -> TypeZ3 b

instance (IsTy a, IsTy b) => IsFun (a -> b) where
instance (IsTy a, IsFun (b -> c)) => IsFun (a -> b -> c) where

instance IsTy a => Compilable (FunApp a) where
  compile = app2AST

app2AST :: IsTy a => FunApp a -> Z3 (Base.AST (TypeZ3 a))
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

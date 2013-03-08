{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- |
-- Module    : Z3.Lang.Monad
-- Copyright : (c) Iago Abal, 2012
--             (c) David Castro, 2012
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>,
--             David Castro <david.castro.dcp@gmail.com>
-- Stability : experimental

module Z3.Lang.Monad (
    -- * Z3 Monad
      Z3
    , Z3State(..)
    , evalZ3
    , Args(..)
    , stdArgs
    , Z3Logic(..)
    , evalZ3With
    , fresh
    , deBruijnIx
    , newQLayout

    -- ** Lifted Z3.Base functions
    , liftZ3
    , assertCnstr
    , check
    , eval
    , getBool
    , getInt
    , getReal
    , getModel
    , delModel
    , withModel
    , getValue
    , mkSort
    , mkStringSymbol
    , mkBoolSort
    , mkIntSort
    , mkRealSort
    , mkLiteral
    , mkNot
    , mkBoolBin
    , mkBoolMulti
    , mkNumeral
    , mkInt
    , mkReal
    , mkPattern
    , mkBound
    , mkForall
    , mkExists
    , mkQuant
    , mkEq
    , mkCmp
    , mkFuncDecl
    , mkApp
    , mkConst
    , mkTrue
    , mkFalse
    , mkUnaryMinus
    , mkCRingArith
    , mkIntArith
    , mkRealArith
    , mkIte
    , pop
    , push
    , showContext
    , showModel
    , astToString
    , setExprPrintMode

    -- * Satisfiability result
    , Base.Result

    ) where

import Z3.Lang.Exprs

import qualified Z3.Base as Base

import Control.Applicative ( Applicative, (<$>) )
import Control.Monad.State
import Data.Traversable ( traverse )

---------------------------------------------------------------------
-- The Z3 Monad

-- | Z3 monad.
--
newtype Z3 a = Z3 (StateT Z3State IO a)
    deriving (Functor, Applicative, Monad, MonadState Z3State)

-- | Internal state of Z3 monad.
--
data Z3State
    = Z3State { uniqVal :: !Uniq
              , context :: Base.Context
              , solver :: Maybe Base.Solver
              , qLayout :: !Layout
              }

-- | Eval a Z3 script.
--
evalZ3 :: Z3 a -> IO a
evalZ3 = evalZ3With stdArgs

-- | Eval a Z3 script.
--
evalZ3With :: Args -> Z3 a -> IO a
evalZ3With args (Z3 s) =
  Base.withConfig $ \cfg -> do
    Base.set_MODEL cfg True
    Base.set_MODEL_PARTIAL cfg False
--    Base.setParamValue cfg "WARNING" "false"
    iniConfig cfg args
    Base.withContext cfg $ \ctx ->
      do msolver <- case logicName args of
                      Just l ->
                        Just <$> Base.mkSolverForLogic ctx (z3logic2String l)
                      _ -> return Nothing
         evalStateT s Z3State { uniqVal = 0
                              , context = ctx
                              , solver = msolver
                              , qLayout = 0
                              }

-- | Fresh symbol name.
--
fresh :: Z3 (Uniq, String)
fresh = do
    st <- get
    let i = uniqVal st
    put st { uniqVal = i + 1 }
    return (uniqVal st, 'v':show i)

-------------------------------------------------
-- Solvers available in Z3
--
-- NOTE: These are described at http://smtlib.cs.uiowa.edu/logics.html

data Z3Logic
  = Logic_AUFLIA
    -- ^ Closed formulas over the theory of linear integer arithmetic
    -- and arrays extended with free sort and function symbols but
    -- restricted to arrays with integer indices and values.

  | Logic_AUFLIRA
    -- ^ Closed linear formulas with free sort and function symbols over
    -- one- and two-dimentional arrays of integer index and real
    -- value.

  | Logic_AUFNIRA
    -- ^ Closed formulas with free function and predicate symbols over a
    -- theory of arrays of arrays of integer index and real value.

  | Logic_LRA
    -- ^ Closed linear formulas in linear real arithmetic.

  | Logic_QF_ABV
    -- ^ Closed quantifier-free formulas over the theory of bitvectors
    -- and bitvector arrays.

  | Logic_QF_AUFBV
    -- ^ Closed quantifier-free formulas over the theory of bitvectors
    -- and bitvector arrays extended with free sort and function
    -- symbols.

  | Logic_QF_AUFLIA
    -- ^ Closed quantifier-free linear formulas over the theory of
    -- integer arrays extended with free sort and function symbols.

  | Logic_QF_AX
    -- ^ Closed quantifier-free formulas over the theory of arrays with
    -- extensionality.

  | Logic_QF_BV
    -- ^ Closed quantifier-free formulas over the theory of fixed-size
    -- bitvectors.

  | Logic_QF_IDL
    -- ^ Difference Logic over the integers. In essence, Boolean
    -- combinations of inequations of the form x - y < b where x and y
    -- are integer variables and b is an integer constant.

  | Logic_QF_LIA
    -- ^ Unquantified linear integer arithmetic. In essence, Boolean
    -- combinations of inequations between linear polynomials over
    -- integer variables.

  | Logic_QF_LRA
    -- ^ Unquantified linear real arithmetic. In essence, Boolean
    -- combinations of inequations between linear polynomials over
    -- real variables.

  | Logic_QF_NIA
    -- ^ Quantifier-free integer arithmetic.

  | Logic_QF_NRA
    -- ^ Quantifier-free real arithmetic.

  | Logic_QF_RDL
    -- ^ Difference Logic over the reals. In essence, Boolean
    -- combinations of inequations of the form x - y < b where x and y
    -- are real variables and b is a rational constant.

  | Logic_QF_UF
    -- ^ Unquantified formulas built over a signature of uninterpreted
    -- (i.e., free) sort and function symbols.

  | Logic_QF_UFBV
    -- ^ Unquantified formulas over bitvectors with uninterpreted sort
    -- function and symbols.

  | Logic_QF_UFIDL
    -- ^ Difference Logic over the integers (in essence) but with
    -- uninterpreted sort and function symbols.

  | Logic_QF_UFLIA
    -- ^ Unquantified linear integer arithmetic with uninterpreted sort
    -- and function symbols.

  | Logic_QF_UFLRA
    -- ^ Unquantified linear real arithmetic with uninterpreted sort and
    -- function symbols.

  | Logic_QF_UFNRA
    -- ^ Unquantified non-linear real arithmetic with uninterpreted sort
    -- and function symbols.

  | Logic_UFLRA
    -- ^ Linear real arithmetic with uninterpreted sort and function
    -- symbols.

  | Logic_UFNIA
    -- ^ Non-linear integer arithmetic with uninterpreted sort and
    -- function symbols.

z3logic2String :: Z3Logic -> String
z3logic2String Logic_AUFLIA = "AUFLIA"
z3logic2String Logic_AUFLIRA = "AUFLIRA"
z3logic2String Logic_AUFNIRA = "AUFNIRA"
z3logic2String Logic_LRA = "LRA"
z3logic2String Logic_QF_ABV = "QF_ABV"
z3logic2String Logic_QF_AUFBV = "QF_AUFBV"
z3logic2String Logic_QF_AUFLIA = "QF_AUFLIA"
z3logic2String Logic_QF_AX = "QF_AX"
z3logic2String Logic_QF_BV = "QF_BV"
z3logic2String Logic_QF_IDL = "QF_IDL"
z3logic2String Logic_QF_LIA = "QF_LIA"
z3logic2String Logic_QF_LRA = "QF_LRA"
z3logic2String Logic_QF_NIA = "QF_NIA"
z3logic2String Logic_QF_NRA = "QF_NRA"
z3logic2String Logic_QF_RDL = "QF_RDL"
z3logic2String Logic_QF_UF = "QF_UF"
z3logic2String Logic_QF_UFBV = "QF_UFBV"
z3logic2String Logic_QF_UFIDL = "QF_UFIDL"
z3logic2String Logic_QF_UFLIA = "QF_UFLIA"
z3logic2String Logic_QF_UFLRA = "QF_UFLRA"
z3logic2String Logic_QF_UFNRA = "QF_UFNRA"
z3logic2String Logic_UFLRA = "UFLRA"
z3logic2String Logic_UFNIA = "UFNIA"


-------------------------------------------------
-- Arguments

data Args
  = Args {
      softTimeout :: Maybe Int
        -- ^ soft timeout (in milliseconds)
      , logicName :: Maybe Z3Logic
        -- ^ an optional name for the logic to use; logic names are
        -- described at <http://smtlib.cs.uiowa.edu/logics.html>
    }

stdArgs :: Args
stdArgs
  = Args {
      softTimeout = Nothing,
      logicName = Nothing
    }

iniConfig :: Base.Config -> Args -> IO ()
iniConfig cfg args = do
  Base.setParamValue cfg "SOFT_TIMEOUT" soft_timeout_val
  where soft_timeout_val = show $ maybe 0 id $ softTimeout args

-------------------------------------------------
-- HOAX-deBruijn conversion

getQLayout :: Z3 Layout
getQLayout = gets qLayout

deBruijnIx :: Layout -> Z3 Int
deBruijnIx k = do lyt <- getQLayout; return $ lyt-k-1

newQLayout :: (Expr a -> Z3 b) -> Z3 b
newQLayout f = do
  lyt <- getQLayout
  incQLayout
  x <- f (Tag lyt)
  decQLayout
  return x
  where incQLayout = modify (\st@Z3State{qLayout} -> st{ qLayout = qLayout+1 })
        decQLayout = modify (\st@Z3State{qLayout} -> st{ qLayout = qLayout-1 })

---------------------------------------------------------------------
-- Lifted Base functions

liftZ3 :: IO a -> Z3 a
liftZ3 = Z3 . lift

liftZ3Op :: (Base.Context -> IO b) -> Z3 b
liftZ3Op f = liftZ3 . f =<< gets context

liftZ3Op2 :: (Base.Context -> a -> IO b) -> a -> Z3 b
liftZ3Op2 f a = gets context >>= \ctx -> liftZ3 (f ctx a)

liftZ3Op3 :: (Base.Context -> a -> b -> IO c) -> a -> b -> Z3 c
liftZ3Op3 f a b = gets context >>= \ctx -> liftZ3 (f ctx a b)

liftZ3Op4 :: (Base.Context -> a -> b -> c -> IO d) -> a -> b -> c -> Z3 d
liftZ3Op4 f a b c = gets context >>= \ctx -> liftZ3 (f ctx a b c)

liftZ3Op5 :: (Base.Context -> a -> b -> c -> d -> IO e) -> a -> b -> c -> d -> Z3 e
liftZ3Op5 f a b c d = gets context >>= \ctx -> liftZ3 (f ctx a b c d)

liftZ3SolverOp :: (Base.Context -> Base.Solver -> IO b) ->
                  (Base.Context -> IO b) -> Z3 b
liftZ3SolverOp f_s f_no_s =
  do ctx <- gets context
     maybe_solver <- gets solver
     case maybe_solver of
       Just solver -> liftZ3 (f_s ctx solver)
       Nothing -> liftZ3 (f_no_s ctx)

liftZ3SolverOp2 :: (Base.Context -> Base.Solver -> a -> IO b) ->
                   (Base.Context -> a -> IO b) -> a -> Z3 b
liftZ3SolverOp2 f_s f_no_s a =
  liftZ3SolverOp (\c s -> f_s c s a) (\c -> f_no_s c a)

assertCnstr :: Base.AST -> Z3 ()
assertCnstr = liftZ3SolverOp2 Base.solverAssertCnstr Base.assertCnstr

-- | Check satisfiability.
check :: Z3 Base.Result
check = liftZ3SolverOp Base.solverCheck Base.check

eval :: Base.Model -> Base.AST -> Z3 (Maybe Base.AST)
eval = liftZ3Op3 Base.eval

getBool :: Base.AST -> Z3 (Maybe Bool)
getBool = liftZ3Op2 Base.getBool

push :: Z3 ()
push = liftZ3SolverOp Base.solverPush Base.push

pop :: Int -> Z3 ()
pop = liftZ3SolverOp2 Base.solverPop Base.pop

getInt :: Base.AST -> Z3 Integer
getInt = liftZ3Op2 Base.getInt

getReal :: Base.AST -> Z3 Rational
getReal = liftZ3Op2 Base.getReal

getModel :: Z3 (Base.Result, Maybe Base.Model)
getModel = liftZ3SolverOp Base.solverCheckAndGetModel Base.getModel

delModel :: Base.Model -> Z3 ()
delModel = liftZ3Op2 Base.delModel

withModel :: (Base.Model -> Z3 a) -> Z3 (Maybe a)
withModel f = do
 (_,m) <- getModel
 r <- traverse f m
 _ <- traverse delModel m
 return r

showModel :: Z3 (Maybe String)
showModel = withModel (liftZ3Op2 Base.showModel)

astToString :: Base.AST -> Z3 String
astToString = liftZ3Op2 Base.astToString

-- | Set the mode for converting expressions to strings.
setExprPrintMode :: Base.ASTPrintMode -> Z3 ()
setExprPrintMode = liftZ3Op2 Base.setASTPrintMode

showContext :: Z3 String
showContext = do
  c <- gets context
  liftZ3 $ Base.showContext c

mkStringSymbol :: String -> Z3 Base.Symbol
mkStringSymbol = liftZ3Op2 Base.mkStringSymbol

mkBoolSort :: Z3 Base.Sort
mkBoolSort = liftZ3Op Base.mkBoolSort

mkIntSort :: Z3 Base.Sort
mkIntSort = liftZ3Op Base.mkIntSort

mkRealSort :: Z3 Base.Sort
mkRealSort = liftZ3Op Base.mkRealSort

mkNot :: Base.AST -> Z3 Base.AST
mkNot = liftZ3Op2 Base.mkNot

mkBoolBin :: BoolBinOp -> Base.AST -> Base.AST -> Z3 Base.AST
mkBoolBin Xor     = liftZ3Op3 Base.mkXor
mkBoolBin Implies = liftZ3Op3 Base.mkImplies
mkBoolBin Iff     = liftZ3Op3 Base.mkIff

mkBoolMulti :: BoolMultiOp -> [Base.AST] -> Z3 Base.AST
mkBoolMulti And      = liftZ3Op2 Base.mkAnd
mkBoolMulti Or       = liftZ3Op2 Base.mkOr

mkNumeral :: String -> Base.Sort -> Z3 Base.AST
mkNumeral = liftZ3Op3 Base.mkNumeral

mkInt  :: Integral a => a -> Z3 Base.AST
mkInt = liftZ3Op2 Base.mkInt

mkReal :: Real r => r -> Z3 Base.AST
mkReal = liftZ3Op2 Base.mkReal

mkPattern :: [Base.AST] -> Z3 Base.Pattern
mkPattern = liftZ3Op2 Base.mkPattern

mkBound :: Int -> Base.Sort -> Z3 Base.AST
mkBound = liftZ3Op3 Base.mkBound

mkForall :: [Base.Pattern]
            -> [Base.Symbol] -> [Base.Sort]
            -> Base.AST -> Z3 Base.AST
mkForall = liftZ3Op5 Base.mkForall

mkExists :: [Base.Pattern]
            -> [Base.Symbol] -> [Base.Sort]
            -> Base.AST -> Z3 Base.AST
mkExists = liftZ3Op5 Base.mkExists

mkQuant :: Quantifier
           -> [Base.Pattern]
           -> [Base.Symbol] -> [Base.Sort]
           -> Base.AST -> Z3 Base.AST
mkQuant ForAll = mkForall
mkQuant Exists = mkExists

mkEq :: CmpOpE -> [Base.AST] -> Z3 Base.AST
mkEq Distinct = liftZ3Op2 Base.mkDistinct
mkEq Neq      = mkNot <=< mkEq Eq
mkEq Eq       = doMkEq
  where doMkEq [e1,e2]
          = (liftZ3Op3 Base.mkEq) e1 e2
        doMkEq es
          | length es < 2 = error "Z3.Lang.Monad.mkEq:\
              \ Invalid number of parameters."
          | otherwise     = join $ liftM (mkBoolMulti And) doEqs
          where doEqs = mapM (uncurry $ liftZ3Op3 Base.mkEq) pairs
                pairs = init $ zip es $ tail $ cycle es

mkCmp :: CmpOpI -> Base.AST -> Base.AST -> Z3 Base.AST
mkCmp Le = liftZ3Op3 Base.mkLe
mkCmp Lt = liftZ3Op3 Base.mkLt
mkCmp Ge = liftZ3Op3 Base.mkGe
mkCmp Gt = liftZ3Op3 Base.mkGt

mkFuncDecl :: Base.Symbol -> [Base.Sort] -> Base.Sort -> Z3 Base.FuncDecl
mkFuncDecl = liftZ3Op4 Base.mkFuncDecl

mkApp :: Base.FuncDecl -> [Base.AST] -> Z3 Base.AST
mkApp = liftZ3Op3 Base.mkApp

mkConst :: Base.Symbol -> Base.Sort -> Z3 Base.AST
mkConst = liftZ3Op3 Base.mkConst

mkTrue :: Z3 Base.AST
mkTrue = liftZ3Op Base.mkTrue

mkFalse :: Z3 Base.AST
mkFalse = liftZ3Op Base.mkFalse

mkUnaryMinus :: Base.AST -> Z3 Base.AST
mkUnaryMinus = liftZ3Op2 Base.mkUnaryMinus

mkCRingArith :: CRingOp -> [Base.AST] -> Z3 Base.AST
mkCRingArith Add = liftZ3Op2 Base.mkAdd
mkCRingArith Mul = liftZ3Op2 Base.mkMul
mkCRingArith Sub = liftZ3Op2 Base.mkSub

mkIntArith :: IntOp -> Base.AST -> Base.AST -> Z3 Base.AST
mkIntArith Quot = liftZ3Op3 Base.mkDiv
mkIntArith Mod  = liftZ3Op3 Base.mkMod
mkIntArith Rem  = liftZ3Op3 Base.mkRem

mkRealArith :: RealOp -> Base.AST -> Base.AST -> Z3 Base.AST
mkRealArith Div = liftZ3Op3 Base.mkDiv

mkIte :: Base.AST -> Base.AST -> Base.AST -> Z3 Base.AST
mkIte = liftZ3Op4 Base.mkIte

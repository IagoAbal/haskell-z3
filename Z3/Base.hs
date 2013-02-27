{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE TupleSections              #-}

-- |
-- Module    : Z3.Base
-- Copyright : (c) Iago Abal, 2012-2013
--             (c) David Castro, 2012-2013
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>,
--             David Castro <david.castro.dcp@gmail.com>
--
-- Medium-level bindings.

module Z3.Base (

    -- * Types
      Config
    , Context
    , Symbol
    , AST
    , Sort
    , FuncDecl
    , App
    , Pattern
    , Model
    , Params
    , Solver

    -- ** Satisfiability result
    , Result(..)

    -- * Configuration
    , mkConfig
    , delConfig
    , withConfig
    , setParamValue
    , set_MODEL
    , set_MODEL_PARTIAL
    , set_WELL_SORTED_CHECK

    -- * Context
    , mkContext
    , delContext
    , withContext

    -- * Symbols
    , mkStringSymbol

    -- * Sorts
    , mkBoolSort
    , mkIntSort
    , mkRealSort
    , mkBvSort

    -- * Constants and Applications
    , mkFuncDecl
    , mkApp
    , mkConst
    , mkTrue
    , mkFalse
    , mkEq
    , mkNot
    , mkIte
    , mkIff
    , mkImplies
    , mkXor
    , mkAnd
    , mkOr
    , mkDistinct
    , mkAdd
    , mkMul
    , mkSub
    , mkUnaryMinus
    , mkDiv
    , mkMod
    , mkRem
    , mkLt
    , mkLe
    , mkGt
    , mkGe
    , mkInt2Real
    , mkReal2Int
    , mkIsInt

    -- * Bit-vectors
    , mkBvnot
    , mkBvredand
    , mkBvredor
    , mkBvand
    , mkBvor
    , mkBvxor
    , mkBvnand
    , mkBvnor
    , mkBvxnor
    , mkBvneg
    , mkBvadd
    , mkBvsub
    , mkBvmul
    , mkBvudiv
    , mkBvsdiv
    , mkBvurem
    , mkBvsrem
    , mkBvsmod
    , mkBvult
    , mkBvslt
    , mkBvule
    , mkBvsle
    , mkBvuge
    , mkBvsge
    , mkBvugt
    , mkBvsgt
    , mkConcat
    , mkExtract
    , mkSignExt
    , mkZeroExt
    , mkRepeat
    , mkBvshl
    , mkBvlshr
    , mkBvashr
    , mkRotateLeft
    , mkRotateRight
    , mkExtRotateLeft
    , mkExtRotateRight
    , mkInt2bv
    , mkBv2int
    , mkBvnegNoOverflow
    , mkBvaddNoOverflow
    , mkBvaddNoUnderflow
    , mkBvsubNoOverflow
    , mkBvsubNoUnderflow
    , mkBvmulNoOverflow
    , mkBvmulNoUnderflow
    , mkBvsdivNoOverflow

    -- * Numerals
    , mkNumeral
    , mkInt
    , mkReal

    -- * Quantifiers
    , mkPattern
    , mkBound
    , mkForall
    , mkExists

    -- * Accessors
    , getBvSortSize
    , getSort
    , getBool
    , getInt
    , getReal

    -- * Models
    , eval
    , showModel
    , showContext

    -- * Constraints
    , assertCnstr
    , check
    , getModel
    , delModel
    , push
    , pop


    -- * Parameters
    , mkParams
    , paramsSetBool
    , paramsSetUInt
    , paramsSetDouble
    , paramsSetSymbol
    , paramsToString

    -- * Solvers
    , mkSolver
    , mkSimpleSolver
    , mkSolverForLogic
    , solverSetParams
    , solverPush
    , solverPop
    , solverReset
    , solverAssertCnstr
    , solverAssertAndTrack
    , solverCheck
    --, solverGetModel
    , solverCheckAndGetModel
    , solverGetReasonUnknown
    ) where

import Z3.Base.C

import Control.Applicative ( (<$>) )
import Control.Exception ( bracket )
import Data.List ( genericLength )
import Data.Int
import Data.Ratio ( Ratio, numerator, denominator, (%) )
import Data.Typeable ( Typeable )
import Data.Word
import Foreign hiding ( toBool )
import Foreign.C
  ( CInt, CUInt, CLLong, CULLong
  , peekCString
  , withCString )

---------------------------------------------------------------------
-- Types

-- | A Z3 /configuration object/.
newtype Config = Config { unConfig :: Ptr Z3_config }
    deriving Eq

-- | A Z3 /logical context/.
newtype Context = Context { unContext :: Ptr Z3_context }
    deriving Eq

-- | A Z3 /Lisp-link symbol/.
newtype Symbol = Symbol { unSymbol :: Ptr Z3_symbol }
    deriving (Eq, Ord, Show, Storable)

-- | A Z3 /AST node/.
newtype AST = AST { unAST :: Ptr Z3_ast }
    deriving (Eq, Ord, Show, Storable, Typeable)

-- | Kind of Z3 AST representing /types/.
newtype Sort = Sort { unSort :: Ptr Z3_sort }
    deriving (Eq, Ord, Show, Storable)

-- | Kind of AST used to represent function symbols.
newtype FuncDecl = FuncDecl { unFuncDecl :: Ptr Z3_func_decl }
    deriving (Eq, Ord, Show, Storable, Typeable)

-- | A kind of Z3 AST used to represent constant and function declarations.
newtype App = App { _unApp :: Ptr Z3_app }
    deriving (Eq, Ord, Show, Storable)

-- | A kind of AST used to represent pattern and multi-patterns used to
--   guide quantifier instantiation.
newtype Pattern = Pattern { unPattern :: Ptr Z3_pattern }
    deriving (Eq, Ord, Show, Storable)

-- | A model for the constraints asserted into the logical context.
newtype Model = Model { unModel :: Ptr Z3_model }
    deriving Eq

-- | A Z3 parameter set. Starting at Z3 4.0, parameter sets are used
-- to configure many components such as: simplifiers, tactics,
-- solvers, etc.
newtype Params = Params { unParams :: Ptr Z3_params }
    deriving Eq

-- | A Z3 solver engine
newtype Solver = Solver { unSolver :: Ptr Z3_solver }
    deriving Eq

-- | Result of a satisfiability check.
data Result
    = Sat
    | Unsat
    | Undef
    deriving (Eq, Ord, Read, Show)

-- | Convert 'Z3_lbool' from Z3.Base.C to 'Result'
toResult :: Z3_lbool -> Result
toResult lb
    | lb == z3_l_true  = Sat
    | lb == z3_l_false = Unsat
    | lb == z3_l_undef = Undef
    | otherwise        = error "Z3.Base.toResult: illegal `Z3_lbool' value"

-- | Convert 'Z3_bool' to 'Bool'.
--
-- 'Foreign.toBool' should be OK but this is more convenient.
toBool :: Z3_bool -> Bool
toBool b
    | b == z3_true  = True
    | b == z3_false = False
    | otherwise     = error "Z3.Base.toBool: illegal `Z3_bool' value"

-- | Convert 'Bool' to 'Z3_bool'.
unBool :: Bool -> Z3_bool
unBool True  = z3_true
unBool False = z3_false

---------------------------------------------------------------------
-- Configuration

-- | Create a configuration.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga7d6c40d9b79fe8a8851cc8540970787f>
mkConfig :: IO Config
mkConfig = Config <$> z3_mk_config

-- | Delete a configuration.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5e620acf5d55d0271097c9bb97219774>
delConfig :: Config -> IO ()
delConfig = z3_del_config . unConfig

-- | Run a computation using a temporally created configuration.
withConfig :: (Config -> IO a) -> IO a
withConfig = bracket mkConfig delConfig

-- | Set a configuration parameter.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga001ade87a1671fe77d7e53ed0f4f1ec3>
--
-- See: <http://research.microsoft.com/en-us/um/redmond/projects/z3/config.html>
setParamValue :: Config -> String -> String -> IO ()
setParamValue cfg s1 s2 =
  withCString s1  $ \cs1 ->
    withCString s2  $ \cs2 ->
      z3_set_param_value (unConfig cfg) cs1 cs2

-- | Set the /MODEL/ configuration parameter.
--
-- default: 'True', enable/disable model construction.
set_MODEL :: Config -> Bool -> IO ()
set_MODEL cfg True  = setParamValue cfg "MODEL" "true"
set_MODEL cfg False = setParamValue cfg "MODEL" "false"

-- | Set the /MODEL_PARTIAL/ configuration parameter.
--
-- default: 'False', enable/disable partial function interpretations.
set_MODEL_PARTIAL :: Config -> Bool -> IO ()
set_MODEL_PARTIAL cfg True  = setParamValue cfg "MODEL_PARTIAL" "true"
set_MODEL_PARTIAL cfg False = setParamValue cfg "MODEL_PARTIAL" "false"

-- | Set the /WELL_SORTED_CHECK/ configuration parameter.
--
-- default: 'True', enable/disable type checker.
set_WELL_SORTED_CHECK :: Config -> Bool -> IO ()
set_WELL_SORTED_CHECK cfg True  = setParamValue cfg "WELL_SORTED_CHECK" "true"
set_WELL_SORTED_CHECK cfg False = setParamValue cfg "WELL_SORTED_CHECK" "false"

---------------------------------------------------------------------
-- Context

-- | Create a context using the given configuration.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0bd93cfab4d749dd3e2f2a4416820a46>
mkContext :: Config -> IO Context
mkContext cfg = Context <$> z3_mk_context (unConfig cfg)

-- | Delete the given logical context.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga556eae80ed43ab13e1e7dc3b38c35200>
delContext :: Context -> IO ()
delContext = z3_del_context . unContext

-- | Run a computation using a temporally created context.
withContext :: Config -> (Context -> IO a) -> IO a
withContext cfg = bracket (mkContext cfg) delContext

---------------------------------------------------------------------
-- Symbols

-- | Create a Z3 symbol using a string.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gafebb0d3c212927cf7834c3a20a84ecae>
mkStringSymbol :: Context -> String -> IO Symbol
mkStringSymbol ctx s =
  withCString s $ \cs ->
    Symbol <$> z3_mk_string_symbol (unContext ctx) cs

---------------------------------------------------------------------
-- Sorts

-- TODO Sorts: Z3_is_eq_sort
-- TODO Sorts: Z3_mk_uninterpreted_sort

-- | Create the /Boolean/ type.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gacdc73510b69a010b71793d429015f342>
mkBoolSort :: Context -> IO Sort
mkBoolSort c = Sort <$> z3_mk_bool_sort (unContext c)

-- | Create an /integer/ type.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga6cd426ab5748653b77d389fd3eac1015>
mkIntSort :: Context -> IO Sort
mkIntSort c = Sort <$> z3_mk_int_sort (unContext c)

-- | Create a /real/ type.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga40ef93b9738485caed6dc84631c3c1a0>
mkRealSort :: Context -> IO Sort
mkRealSort c = Sort <$> z3_mk_real_sort (unContext c)

-- | Create a bit-vector type of the given size.
--
-- This type can also be seen as a machine integer.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaeed000a1bbb84b6ca6fdaac6cf0c1688>
mkBvSort :: Context -> Int -> IO Sort
mkBvSort c n = Sort <$> z3_mk_bv_sort (unContext c) (fromIntegral n)

-- TODO Sorts: from Z3_mk_finite_domain_sort on

---------------------------------------------------------------------
-- Constants and Applications

-- | A Z3 function
mkFuncDecl :: Context -> Symbol -> [Sort] -> Sort -> IO FuncDecl
mkFuncDecl ctx smb dom rng =
  withArray (map unSort dom) $ \c_dom ->
    FuncDecl <$> z3_mk_func_decl (unContext ctx)
                                 (unSymbol smb)
                                 (genericLength dom)
                                 c_dom
                                 (unSort rng)

-- | Create a constant or function application.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga33a202d86bf628bfab9b6f437536cebe>
mkApp :: Context -> FuncDecl -> [AST] -> IO AST
mkApp ctx fd args =
  withArray (map unAST args) $ \pargs ->
    AST <$> z3_mk_app ctxPtr fdPtr numArgs pargs
  where ctxPtr  = unContext ctx
        fdPtr   = unFuncDecl fd
        numArgs = genericLength args

-- | Declare and create a constant.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga093c9703393f33ae282ec5e8729354ef>
mkConst :: Context -> Symbol -> Sort -> IO AST
mkConst c x s = AST <$> z3_mk_const (unContext c) (unSymbol x) (unSort s)

-- TODO Constants and Applications: Z3_mk_fresh_func_decl
-- TODO Constants and Applications: Z3_mk_fresh_const

-- | Create an AST node representing /true/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae898e7380409bbc57b56cc5205ef1db7>
mkTrue :: Context -> IO AST
mkTrue c = AST <$> z3_mk_true (unContext c)

-- | Create an AST node representing /false/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5952ac17671117a02001fed6575c778d>
mkFalse :: Context -> IO AST
mkFalse c = AST <$> z3_mk_false (unContext c)

-- | Create an AST node representing /l = r/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga95a19ce675b70e22bb0401f7137af37c>
mkEq :: Context -> AST -> AST -> IO AST
mkEq c e1 e2 = AST <$> z3_mk_eq (unContext c) (unAST e1) (unAST e2)

-- | The distinct construct is used for declaring the arguments pairwise
-- distinct.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaa076d3a668e0ec97d61744403153ecf7>
mkDistinct :: Context -> [AST] -> IO AST
mkDistinct _ [] = error "Z3.Base.mkDistinct: empty list of expressions"
mkDistinct c es =
  withArray (map unAST es) $ \aptr ->
    AST <$> z3_mk_distinct (unContext c) n aptr
  where n = genericLength es

-- | Create an AST node representing /not(a)/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga3329538091996eb7b3dc677760a61072>
mkNot :: Context -> AST -> IO AST
mkNot c e = AST <$> z3_mk_not (unContext c) (unAST e)

-- | Create an AST node representing an if-then-else: /ite(t1, t2, t3)/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga94417eed5c36e1ad48bcfc8ad6e83547>
mkIte :: Context -> AST -> AST -> AST -> IO AST
mkIte c g e1 e2 =
  AST <$> z3_mk_ite (unContext c) (unAST g) (unAST e1) (unAST e2)

-- | Create an AST node representing /t1 iff t2/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga930a8e844d345fbebc498ac43a696042>
mkIff :: Context -> AST -> AST -> IO AST
mkIff c p q = AST <$> z3_mk_iff (unContext c) (unAST p) (unAST q)

-- | Create an AST node representing /t1 implies t2/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac829c0e25bbbd30343bf073f7b524517>
mkImplies :: Context -> AST -> AST -> IO AST
mkImplies c p q = AST <$> z3_mk_implies (unContext c) (unAST p) (unAST q)

-- | Create an AST node representing /t1 xor t2/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gacc6d1b848032dec0c4617b594d4229ec>
mkXor :: Context -> AST -> AST -> IO AST
mkXor c p q = AST <$> z3_mk_xor (unContext c) (unAST p) (unAST q)

-- | Create an AST node representing args[0] and ... and args[num_args-1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gacde98ce4a8ed1dde50b9669db4838c61>
mkAnd :: Context -> [AST] -> IO AST
mkAnd _ [] = error "Z3.Base.mkAnd: empty list of expressions"
mkAnd c es =
  withArray (map unAST es) $ \aptr ->
    AST <$> z3_mk_and (unContext c) n aptr
  where n = genericLength es

-- | Create an AST node representing args[0] or ... or args[num_args-1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga00866d16331d505620a6c515302021f9>
mkOr :: Context -> [AST] -> IO AST
mkOr _ [] = error "Z3.Base.mkOr: empty list of expressions"
mkOr c es =
  withArray (map unAST es) $ \aptr ->
    AST <$> z3_mk_or (unContext c) n aptr
  where n = genericLength es

-- | Create an AST node representing args[0] + ... + args[num_args-1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4e4ac0a4e53eee0b4b0ef159ed7d0cd5>
mkAdd :: Context -> [AST] -> IO AST
mkAdd _ [] = error "Z3.Base.mkAdd: empty list of expressions"
mkAdd c es =
  withArray (map unAST es) $ \aptr ->
    AST <$> z3_mk_add (unContext c) n aptr
  where n = genericLength es

-- | Create an AST node representing args[0] * ... * args[num_args-1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab9affbf8401a18eea474b59ad4adc890>
mkMul :: Context -> [AST] -> IO AST
mkMul _ [] = error "Z3.Base.mkMul: empty list of expressions"
mkMul c es =
  withArray (map unAST es) $ \aptr ->
    AST <$> z3_mk_mul (unContext c) n aptr
  where n = genericLength es

-- | Create an AST node representing args[0] - ... - args[num_args - 1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4f5fea9b683f9e674fd8f14d676cc9a9>
mkSub :: Context -> [AST] -> IO AST
mkSub _ [] = error "Z3.Base.mkSub: empty list of expressions"
mkSub c es =
  withArray (map unAST es) $ \aptr ->
    AST <$> z3_mk_sub (unContext c) n aptr
  where n = genericLength es

-- | Create an AST node representing -arg.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gadcd2929ad732937e25f34277ce4988ea>
mkUnaryMinus :: Context -> AST -> IO AST
mkUnaryMinus c e = AST <$> z3_mk_unary_minus (unContext c) (unAST e)

-- | Create an AST node representing arg1 div arg2.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1ac60ee8307af8d0b900375914194ff3>
mkDiv :: Context -> AST -> AST -> IO AST
mkDiv c e1 e2 = AST <$> z3_mk_div (unContext c) (unAST e1) (unAST e2)

-- | Create an AST node representing arg1 mod arg2.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga8e350ac77e6b8fe805f57efe196e7713>
mkMod :: Context -> AST -> AST -> IO AST
mkMod c e1 e2 = AST <$> z3_mk_mod (unContext c) (unAST e1) (unAST e2)

-- | Create an AST node representing arg1 rem arg2.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga2fcdb17f9039bbdaddf8a30d037bd9ff>
mkRem :: Context -> AST -> AST -> IO AST
mkRem c e1 e2 = AST <$> z3_mk_rem (unContext c) (unAST e1) (unAST e2)

-- | Create less than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga58a3dc67c5de52cf599c346803ba1534>
mkLt :: Context -> AST -> AST -> IO AST
mkLt c e1 e2 = AST <$> z3_mk_lt (unContext c) (unAST e1) (unAST e2)

-- | Create less than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaa9a33d11096841f4e8c407f1578bc0bf>
mkLe :: Context -> AST -> AST -> IO AST
mkLe c e1 e2 = AST <$> z3_mk_le (unContext c) (unAST e1) (unAST e2)

-- | Create greater than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga46167b86067586bb742c0557d7babfd3>
mkGt :: Context -> AST -> AST -> IO AST
mkGt c e1 e2 = AST <$> z3_mk_gt (unContext c) (unAST e1) (unAST e2)

-- | Create greater than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gad9245cbadb80b192323d01a8360fb942>
mkGe :: Context -> AST -> AST -> IO AST
mkGe c e1 e2 = AST <$> z3_mk_ge (unContext c) (unAST e1) (unAST e2)

-- | Coerce an integer to a real.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga7130641e614c7ebafd28ae16a7681a21>
mkInt2Real :: Context -> AST -> IO AST
mkInt2Real c e = AST <$> z3_mk_int2real (unContext c) (unAST e)

-- | Coerce a real to an integer.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga759b6563ba1204aae55289009a3fdc6d>
mkReal2Int :: Context -> AST -> IO AST
mkReal2Int c e = AST <$> z3_mk_real2int (unContext c) (unAST e)

-- | Check if a real number is an integer.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaac2ad0fb04e4900fdb4add438d137ad3>
mkIsInt :: Context -> AST -> IO AST
mkIsInt c e = AST <$> z3_mk_is_int (unContext c) (unAST e)

---------------------------------------------------------------------
-- Bit-vectors

-- | Bitwise negation.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga36cf75c92c54c1ca633a230344f23080>
mkBvnot :: Context -> AST -> IO AST
mkBvnot c e = AST <$> z3_mk_bvnot (unContext c) (unAST e)

-- | Take conjunction of bits in vector, return vector of length 1.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaccc04f2b58903279b1b3be589b00a7d8>
mkBvredand :: Context -> AST -> IO AST
mkBvredand c e = AST <$> z3_mk_bvredand (unContext c) (unAST e)

-- | Take disjunction of bits in vector, return vector of length 1.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gafd18e127c0586abf47ad9cd96895f7d2>
mkBvredor :: Context -> AST -> IO AST
mkBvredor c e = AST <$> z3_mk_bvredor (unContext c) (unAST e)

-- | Bitwise and.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab96e0ea55334cbcd5a0e79323b57615d>
mkBvand :: Context -> AST -> AST -> IO AST
mkBvand c e1 e2 = AST <$> z3_mk_bvand (unContext c) (unAST e1) (unAST e2)

-- | Bitwise or.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga77a6ae233fb3371d187c6d559b2843f5>
mkBvor :: Context -> AST -> AST -> IO AST
mkBvor c e1 e2 = AST <$> z3_mk_bvor (unContext c) (unAST e1) (unAST e2)

-- | Bitwise exclusive-or.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0a3821ea00b1c762205f73e4bc29e7d8>
mkBvxor :: Context -> AST -> AST -> IO AST
mkBvxor c e1 e2 = AST <$> z3_mk_bvxor (unContext c) (unAST e1) (unAST e2)

-- | Bitwise nand.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga96dc37d36efd658fff5b2b4df49b0e61>
mkBvnand :: Context -> AST -> AST -> IO AST
mkBvnand c e1 e2 = AST <$> z3_mk_bvnand (unContext c) (unAST e1) (unAST e2)

-- | Bitwise nor.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gabf15059e9e8a2eafe4929fdfd259aadb>
mkBvnor :: Context -> AST -> AST -> IO AST
mkBvnor c e1 e2 = AST <$> z3_mk_bvnor (unContext c) (unAST e1) (unAST e2)

-- | Bitwise xnor.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga784f5ca36a4b03b93c67242cc94b21d6>
mkBvxnor :: Context -> AST -> AST -> IO AST
mkBvxnor c e1 e2 = AST <$> z3_mk_bvxnor (unContext c) (unAST e1) (unAST e2)

-- | Standard two's complement unary minus.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0c78be00c03eda4ed6a983224ed5c7b7
mkBvneg :: Context -> AST -> IO AST
mkBvneg c e = AST <$> z3_mk_bvneg (unContext c) (unAST e)

-- | Standard two's complement addition.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga819814e33573f3f9948b32fdc5311158>
mkBvadd :: Context -> AST -> AST -> IO AST
mkBvadd c e1 e2 = AST <$> z3_mk_bvadd (unContext c) (unAST e1) (unAST e2)

-- | Standard two's complement subtraction.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga688c9aa1347888c7a51be4e46c19178e>
mkBvsub :: Context -> AST -> AST -> IO AST
mkBvsub c e1 e2 = AST <$> z3_mk_bvsub (unContext c) (unAST e1) (unAST e2)

-- | Standard two's complement multiplication.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga6abd3dde2a1ceff1704cf7221a72258c>
mkBvmul :: Context -> AST -> AST -> IO AST
mkBvmul c e1 e2 = AST <$> z3_mk_bvmul (unContext c) (unAST e1) (unAST e2)

-- | Unsigned division.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga56ce0cd61666c6f8cf5777286f590544>
mkBvudiv :: Context -> AST -> AST -> IO AST
mkBvudiv c e1 e2 = AST <$> z3_mk_bvudiv (unContext c) (unAST e1) (unAST e2)

-- | Two's complement signed division.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gad240fedb2fda1c1005b8e9d3c7f3d5a0>
mkBvsdiv :: Context -> AST -> AST -> IO AST
mkBvsdiv c e1 e2 = AST <$> z3_mk_bvsdiv (unContext c) (unAST e1) (unAST e2)

-- | Unsigned remainder.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5df4298ec835e43ddc9e3e0bae690c8d>
mkBvurem :: Context -> AST -> AST -> IO AST
mkBvurem c e1 e2 = AST <$> z3_mk_bvurem (unContext c) (unAST e1) (unAST e2)

-- | Two's complement signed remainder (sign follows dividend).
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga46c18a3042fca174fe659d3185693db1>
mkBvsrem :: Context -> AST -> AST -> IO AST
mkBvsrem c e1 e2 = AST <$> z3_mk_bvsrem (unContext c) (unAST e1) (unAST e2)

-- | Two's complement signed remainder (sign follows divisor).
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga95dac8e6eecb50f63cb82038560e0879>
mkBvsmod :: Context -> AST -> AST -> IO AST
mkBvsmod c e1 e2 = AST <$> z3_mk_bvsmod (unContext c) (unAST e1) (unAST e2)

-- | Unsigned less than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5774b22e93abcaf9b594672af6c7c3c4>
mkBvult :: Context -> AST -> AST -> IO AST
mkBvult c e1 e2 = AST <$> z3_mk_bvult (unContext c) (unAST e1) (unAST e2)

-- | Two's complement signed less than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga8ce08af4ed1fbdf08d4d6e63d171663a>
mkBvslt :: Context -> AST -> AST -> IO AST
mkBvslt c e1 e2 = AST <$> z3_mk_bvslt (unContext c) (unAST e1) (unAST e2)

-- | Unsigned less than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab738b89de0410e70c089d3ac9e696e87>
mkBvule :: Context -> AST -> AST -> IO AST
mkBvule c e1 e2 = AST <$> z3_mk_bvule (unContext c) (unAST e1) (unAST e2)

-- | Two's complement signed less than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab7c026feb93e7d2eab180e96f1e6255d>
mkBvsle :: Context -> AST -> AST -> IO AST
mkBvsle c e1 e2 = AST <$> z3_mk_bvsle (unContext c) (unAST e1) (unAST e2)

-- | Unsigned greater than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gade58fbfcf61b67bf8c4a441490d3c4df>
mkBvuge :: Context -> AST -> AST -> IO AST
mkBvuge c e1 e2 = AST <$> z3_mk_bvuge (unContext c) (unAST e1) (unAST e2)

-- | Two's complement signed greater than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaeec3414c0e8a90a6aa5a23af36bf6dc5>
mkBvsge :: Context -> AST -> AST -> IO AST
mkBvsge c e1 e2 = AST <$> z3_mk_bvsge (unContext c) (unAST e1) (unAST e2)

-- | Unsigned greater than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga063ab9f16246c99e5c1c893613927ee3>
mkBvugt :: Context -> AST -> AST -> IO AST
mkBvugt c e1 e2 = AST <$> z3_mk_bvugt (unContext c) (unAST e1) (unAST e2)

-- | Two's complement signed greater than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4e93a985aa2a7812c7c11a2c65d7c5f0>
mkBvsgt :: Context -> AST -> AST -> IO AST
mkBvsgt c e1 e2 = AST <$> z3_mk_bvsgt (unContext c) (unAST e1) (unAST e2)

-- | Concatenate the given bit-vectors.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae774128fa5e9ff7458a36bd10e6ca0fa>
mkConcat :: Context -> AST -> AST -> IO AST
mkConcat c e1 e2 = AST <$> z3_mk_concat (unContext c) (unAST e1) (unAST e2)

-- | Extract the bits high down to low from a bitvector of size m to yield a new
-- bitvector of size /n/, where /n = high - low + 1/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga32d2fe7563f3e6b114c1b97b205d4317>
mkExtract :: Context -> Int -> Int -> AST -> IO AST
mkExtract c j i e
  = AST <$> z3_mk_extract (unContext c) (fromIntegral j) (fromIntegral i) (unAST e)

-- | Sign-extend of the given bit-vector to the (signed) equivalent bitvector
-- of size /m+i/, where /m/ is the size of the given bit-vector.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gad29099270b36d0680bb54b560353c10e>
mkSignExt :: Context -> Int -> AST -> IO AST
mkSignExt c i e
  = AST <$> z3_mk_sign_ext (unContext c) (fromIntegral i) (unAST e)

-- | Extend the given bit-vector with zeros to the (unsigned) equivalent
-- bitvector of size /m+i/, where /m/ is the size of the given bit-vector.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac9322fae11365a78640baf9078c428b3>
mkZeroExt :: Context -> Int -> AST -> IO AST
mkZeroExt c i e
  = AST <$> z3_mk_zero_ext (unContext c) (fromIntegral i) (unAST e)

-- | Repeat the given bit-vector up length /i/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga03e81721502ea225c264d1f556c9119d>
mkRepeat :: Context -> Int -> AST -> IO AST
mkRepeat c i e
  = AST <$> z3_mk_repeat (unContext c) (fromIntegral i) (unAST e)

-- | Shift left.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac8d5e776c786c1172fa0d7dfede454e1>
mkBvshl :: Context -> AST -> AST -> IO AST
mkBvshl c e1 e2 = AST <$> z3_mk_bvshl (unContext c) (unAST e1) (unAST e2)

-- | Logical shift right.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac59645a6edadad79a201f417e4e0c512>
mkBvlshr :: Context -> AST -> AST -> IO AST
mkBvlshr c e1 e2 = AST <$> z3_mk_bvlshr (unContext c) (unAST e1) (unAST e2)

-- | Arithmetic shift right.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga674b580ad605ba1c2c9f9d3748be87c4>
mkBvashr :: Context -> AST -> AST -> IO AST
mkBvashr c e1 e2 = AST <$> z3_mk_bvashr (unContext c) (unAST e1) (unAST e2)

-- | Rotate bits of /t1/ to the left /i/ times.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4932b7d08fea079dd903cd857a52dcda>
mkRotateLeft :: Context -> Int -> AST -> IO AST
mkRotateLeft c i e
  = AST <$> z3_mk_rotate_left (unContext c) (fromIntegral i) (unAST e)

-- | Rotate bits of /t1/ to the right /i/ times.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga3b94e1bf87ecd1a1858af8ebc1da4a1c>
mkRotateRight :: Context -> Int -> AST -> IO AST
mkRotateRight c i e
  = AST <$> z3_mk_rotate_right (unContext c) (fromIntegral i) (unAST e)

-- | Rotate bits of /t1/ to the left /t2/ times.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaf46f1cb80e5a56044591a76e7c89e5e7>
mkExtRotateLeft :: Context -> AST -> AST -> IO AST
mkExtRotateLeft c e1 e2
  = AST <$> z3_mk_ext_rotate_left (unContext c) (unAST e1) (unAST e2)

-- | Rotate bits of /t1/ to the right /t2/ times.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gabb227526c592b523879083f12aab281f>
mkExtRotateRight :: Context -> AST -> AST -> IO AST
mkExtRotateRight c e1 e2
  = AST <$> z3_mk_ext_rotate_right (unContext c) (unAST e1) (unAST e2)

-- | Create an /n/ bit bit-vector from the integer argument /t1/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga35f89eb05df43fbd9cce7200cc1f30b5>
mkInt2bv :: Context -> Int -> AST -> IO AST
mkInt2bv c i e
  = AST <$> z3_mk_int2bv (unContext c) (fromIntegral i) (unAST e)

-- | Create an integer from the bit-vector argument /t1/. If /is_signed/ is false,
-- then the bit-vector /t1/ is treated as unsigned. So the result is non-negative
-- and in the range [0..2^/N/-1], where /N/ are the number of bits in /t1/.
-- If /is_signed/ is true, /t1/ is treated as a signed bit-vector.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac87b227dc3821d57258d7f53a28323d4>
mkBv2int :: Context -> AST -> Bool -> IO AST
mkBv2int c e is_signed
  = AST <$> z3_mk_bv2int (unContext c) (unAST e) (unBool is_signed)

-- | Create a predicate that checks that the bit-wise addition of /t1/ and /t2/
-- does not overflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga88f6b5ec876f05e0d7ba51e96c4b077f>
mkBvaddNoOverflow :: Context -> AST -> AST -> Bool -> IO AST
mkBvaddNoOverflow c e1 e2 is_signed =
  AST <$> z3_mk_bvadd_no_overflow (unContext c) (unAST e1) (unAST e2)
                                  (unBool is_signed)

-- | Create a predicate that checks that the bit-wise signed addition of /t1/
-- and /t2/ does not underflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1e2b1927cf4e50000c1600d47a152947>
mkBvaddNoUnderflow :: Context -> AST -> AST -> IO AST
mkBvaddNoUnderflow c e1 e2 =
  AST <$> z3_mk_bvadd_no_underflow (unContext c) (unAST e1) (unAST e2)

-- | Create a predicate that checks that the bit-wise signed subtraction of /t1/
-- and /t2/ does not overflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga785f8127b87e0b42130e6d8f52167d7c>
mkBvsubNoOverflow :: Context -> AST -> AST -> IO AST
mkBvsubNoOverflow c e1 e2 =
  AST <$> z3_mk_bvsub_no_overflow (unContext c) (unAST e1) (unAST e2)

-- | Create a predicate that checks that the bit-wise subtraction of /t1/ and
-- /t2/ does not underflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga6480850f9fa01e14aea936c88ff184c4>
mkBvsubNoUnderflow :: Context -> AST -> AST -> IO AST
mkBvsubNoUnderflow c e1 e2 =
  AST <$> z3_mk_bvsub_no_underflow (unContext c) (unAST e1) (unAST e2)

-- | Create a predicate that checks that the bit-wise signed division of /t1/
-- and /t2/ does not overflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaa17e7b2c33dfe2abbd74d390927ae83e>
mkBvsdivNoOverflow :: Context -> AST -> AST -> IO AST
mkBvsdivNoOverflow c e1 e2 =
  AST <$> z3_mk_bvsdiv_no_overflow (unContext c) (unAST e1) (unAST e2)

-- | Check that bit-wise negation does not overflow when /t1/ is interpreted as
-- a signed bit-vector.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae9c5d72605ddcd0e76657341eaccb6c7>
mkBvnegNoOverflow :: Context -> AST -> IO AST
mkBvnegNoOverflow c e =
  AST <$> z3_mk_bvneg_no_overflow (unContext c) (unAST e)

-- | Create a predicate that checks that the bit-wise multiplication of /t1/ and
-- /t2/ does not overflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga86f4415719d295a2f6845c70b3aaa1df>
mkBvmulNoOverflow :: Context -> AST -> AST -> Bool -> IO AST
mkBvmulNoOverflow c e1 e2 is_signed =
  AST <$> z3_mk_bvmul_no_overflow (unContext c) (unAST e1) (unAST e2)
                                  (unBool is_signed)

-- | Create a predicate that checks that the bit-wise signed multiplication of
-- /t1/ and /t2/ does not underflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga501ccc01d737aad3ede5699741717fda>
mkBvmulNoUnderflow :: Context -> AST -> AST -> IO AST
mkBvmulNoUnderflow c e1 e2 =
  AST <$> z3_mk_bvmul_no_underflow (unContext c) (unAST e1) (unAST e2)

-- TODO Arrays, Sets

---------------------------------------------------------------------
-- Numerals

-- | Create a numeral of a given sort.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac8aca397e32ca33618d8024bff32948c>
mkNumeral :: Context -> String -> Sort -> IO AST
mkNumeral c str s =
  withCString str $ \cstr->
    AST <$> z3_mk_numeral (unContext c) cstr (unSort s)

-------------------------------------------------
-- Numerals / Integers

-- | Create a numeral of sort /int/.
mkInt :: Integral a => Context -> a -> IO AST
mkInt c n = mkIntSort c >>= mkNumeral c n_str
  where n_str = show $ toInteger n

{-# INLINE mkIntZ3 #-}
mkIntZ3 :: Context -> Int32 -> Sort -> IO AST
mkIntZ3 c n s = AST <$> z3_mk_int (unContext c) cn (unSort s)
  where cn = fromIntegral n :: CInt

{-# INLINE mkUnsignedIntZ3 #-}
mkUnsignedIntZ3 :: Context -> Word32 -> Sort -> IO AST
mkUnsignedIntZ3 c n s = AST <$> z3_mk_unsigned_int (unContext c) cn (unSort s)
  where cn = fromIntegral n :: CUInt

{-# INLINE mkInt64Z3 #-}
mkInt64Z3 :: Context -> Int64 -> Sort -> IO AST
mkInt64Z3 c n s = AST <$> z3_mk_int64 (unContext c) cn (unSort s)
  where cn = fromIntegral n :: CLLong

{-# INLINE mkUnsignedInt64Z3 #-}
mkUnsignedInt64Z3 :: Context -> Word64 -> Sort -> IO AST
mkUnsignedInt64Z3 c n s =
  AST <$> z3_mk_unsigned_int64 (unContext c) cn (unSort s)
  where cn = fromIntegral n :: CULLong

{-# RULES "mkInt/mkInt_IntZ3" mkInt = mkInt_IntZ3 #-}
mkInt_IntZ3 :: Context -> Int32 -> IO AST
mkInt_IntZ3 c n = mkIntSort c >>= mkIntZ3 c n

{-# RULES "mkInt/mkInt_UnsignedIntZ3" mkInt = mkInt_UnsignedIntZ3 #-}
mkInt_UnsignedIntZ3 :: Context -> Word32 -> IO AST
mkInt_UnsignedIntZ3 c n = mkIntSort c >>= mkUnsignedIntZ3 c n

{-# RULES "mkInt/mkInt_Int64Z3" mkInt = mkInt_Int64Z3 #-}
mkInt_Int64Z3 :: Context -> Int64 -> IO AST
mkInt_Int64Z3 c n = mkIntSort c >>= mkInt64Z3 c n

{-# RULES "mkInt/mkInt_UnsignedInt64Z3" mkInt = mkInt_UnsignedInt64Z3 #-}
mkInt_UnsignedInt64Z3 :: Context -> Word64 -> IO AST
mkInt_UnsignedInt64Z3 c n = mkIntSort c >>= mkUnsignedInt64Z3 c n

-------------------------------------------------
-- Numerals / Reals

-- | Create a numeral of sort /real/.
mkReal :: Real r => Context -> r -> IO AST
mkReal c n = mkRealSort c >>= mkNumeral c n_str
  where r = toRational n
        r_n = toInteger $ numerator r
        r_d = toInteger $ denominator r
        n_str = show r_n ++ " / " ++ show r_d

{-# RULES "mkReal/mkRealZ3" mkReal = mkRealZ3 #-}
mkRealZ3 :: Context -> Ratio Int32 -> IO AST
mkRealZ3 c r = AST <$> z3_mk_real (unContext c) n d
  where n = (fromIntegral $ numerator r)   :: CInt
        d = (fromIntegral $ denominator r) :: CInt

{-# RULES "mkReal/mkReal_IntZ3" mkReal = mkReal_IntZ3 #-}
mkReal_IntZ3 :: Context -> Int32 -> IO AST
mkReal_IntZ3 c n = mkRealSort c >>= mkIntZ3 c n

{-# RULES "mkReal/mkReal_UnsignedIntZ3" mkReal = mkReal_UnsignedIntZ3 #-}
mkReal_UnsignedIntZ3 :: Context -> Word32 -> IO AST
mkReal_UnsignedIntZ3 c n = mkRealSort c >>= mkUnsignedIntZ3 c n

{-# RULES "mkReal/mkReal_Int64Z3" mkReal = mkReal_Int64Z3 #-}
mkReal_Int64Z3 :: Context -> Int64 -> IO AST
mkReal_Int64Z3 c n = mkRealSort c >>= mkInt64Z3 c n

{-# RULES "mkReal/mkReal_UnsignedInt64Z3" mkReal = mkReal_UnsignedInt64Z3 #-}
mkReal_UnsignedInt64Z3 :: Context -> Word64 -> IO AST
mkReal_UnsignedInt64Z3 c n = mkRealSort c >>= mkUnsignedInt64Z3 c n

---------------------------------------------------------------------
-- Quantifiers

mkPattern :: Context -> [AST] -> IO Pattern
mkPattern _ [] = error "Z3.Base.mkPattern: empty list of expressions"
mkPattern c es =
  withArray (map unAST es) $ \aptr ->
    Pattern <$> z3_mk_pattern (unContext c) n aptr
  where n = genericLength es

mkBound :: Context -> Int -> Sort -> IO AST
mkBound c i s
  | i >= 0    = AST <$> z3_mk_bound (unContext c) (fromIntegral i) (unSort s)
  | otherwise = error "Z3.Base.mkBound: negative de-Bruijn index"

mkForall :: Context -> [Pattern] -> [Symbol] -> [Sort] -> AST -> IO AST
mkForall c pats x s p
  = withArray (map unPattern pats) $ \patsPtr ->
    withArray (map unSymbol  x   ) $ \xptr ->
    withArray (map unSort    s   ) $ \sptr ->
      AST <$> z3_mk_forall cptr 0 n patsPtr len sptr xptr (unAST p)
  where n    = genericLength pats
        cptr = unContext c
        len
          | l == 0        = error "Z3.Base.mkForall:\
              \ forall with 0 bound variables"
          | l /= length x = error "Z3.Base.mkForall:\
              \ different number of symbols and sorts"
          | otherwise     = fromIntegral l
          where l = length s

mkExists :: Context -> [Pattern] -> [Symbol] -> [Sort] -> AST -> IO AST
mkExists c pats x s p
  = withArray (map unPattern pats) $ \patsPtr ->
    withArray (map unSymbol  x   ) $ \xptr ->
    withArray (map unSort    s   ) $ \sptr ->
      AST <$> z3_mk_exists cptr 0 n patsPtr len sptr xptr (unAST p)
  where n    = fromIntegral $ length pats
        cptr = unContext c
        len
          | l == 0        = error "Z3.Base.mkForall:\
              \ forall with 0 bound variables"
          | l /= length x = error "Z3.Base.mkForall:\
              \ different number of symbols and sorts"
          | otherwise     = fromIntegral l
          where l = length s

---------------------------------------------------------------------
-- Accessors

-- | Return the size of the given bit-vector sort.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga8fc3550edace7bc046e16d1f96ddb419>
getBvSortSize :: Context -> Sort -> IO Int
getBvSortSize c s =
  fromIntegral <$> z3_get_bv_sort_size (unContext c) (unSort s)

-- | Return the sort of an AST node.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0a4dac7e9397ff067136354cd33cb933>
getSort :: Context -> AST -> IO Sort
getSort c e = Sort <$> z3_get_sort (unContext c) (unAST e)

-- | Cast a 'Z3_lbool' from Z3.Base.C to @Bool@.
castLBool :: Z3_lbool -> Maybe Bool
castLBool lb
    | lb == z3_l_true  = Just True
    | lb == z3_l_false = Just False
    | lb == z3_l_undef = Nothing
    | otherwise        = error "Z3.Base.castLBool: illegal `Z3_lbool' value"

-- | Return Z3_L_TRUE if a is true, Z3_L_FALSE if it is false, and Z3_L_UNDEF
-- otherwise.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga133aaa1ec31af9b570ed7627a3c8c5a4>
getBool :: Context -> AST -> IO (Maybe Bool)
getBool c a = castLBool <$> z3_get_bool_value (unContext c) (unAST a)

-- | Return numeral value, as a string of a numeric constant term.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga94617ef18fa7157e1a3f85db625d2f4b>
getNumeralString :: Context -> AST -> IO String
getNumeralString c a = peekCString =<< z3_get_numeral_string ctxPtr (unAST a)
  where ctxPtr = unContext c

-- | Return 'Z3Int' value
getInt :: Context -> AST -> IO Integer
getInt c a = read <$> getNumeralString c a

-- | Return 'Z3Real' value
getReal :: Context -> AST -> IO Rational
getReal c a = parse <$> getNumeralString c a
  where parse :: String -> Rational
        parse s
          | [(i, sj)] <- reads s = i % parseDen (dropWhile (== ' ') sj)
          | otherwise            = error "Z3.Base.getReal: no parse"

        parseDen :: String -> Integer
        parseDen ""       = 1
        parseDen ('/':sj) = read sj
        parseDen _        = error "Z3.Base.getReal: no parse"


-- TODO Modifiers

---------------------------------------------------------------------
-- Models

-- | Evaluate an AST node in the given model.
eval :: Context -> Model -> AST -> IO (Maybe AST)
eval ctx m a =
  alloca $ \aptr2 ->
    z3_eval ctxPtr (unModel m) (unAST a) aptr2 >>= peekAST aptr2 . toBool
  where peekAST :: Ptr (Ptr Z3_ast) -> Bool -> IO (Maybe AST)
        peekAST _p False = return Nothing
        peekAST  p True  = Just . AST <$> peek p

        ctxPtr = unContext ctx

---------------------------------------------------------------------
-- Constraints

push :: Context -> IO ()
push = z3_push . unContext

pop :: Context -> Int -> IO ()
pop ctx cnt = z3_pop (unContext ctx) $ fromIntegral cnt

-- TODO Constraints: Z3_get_num_scopes
-- TODO Constraints: Z3_persist_ast

-- | Assert a constraing into the logical context.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1a05ff73a564ae7256a2257048a4680a>
assertCnstr :: Context -> AST -> IO ()
assertCnstr ctx ast = z3_assert_cnstr (unContext ctx) (unAST ast)

-- | Get model (logical context is consistent)
--
-- Reference : <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaff310fef80ac8a82d0a51417e073ec0a>
getModel :: Context -> IO (Result, Maybe Model)
getModel c =
  alloca $ \mptr ->
    z3_check_and_get_model (unContext c) mptr >>= \lb ->
      (toResult lb,) <$> peekModel mptr
  where peekModel :: Ptr (Ptr Z3_model) -> IO (Maybe Model)
        peekModel p | p == nullPtr = return Nothing
                    | otherwise    = mkModel <$> peek p
        mkModel :: Ptr Z3_model -> Maybe Model
        mkModel p | p == nullPtr = Nothing
                  | otherwise    = Just $ Model p

-- | Delete a model object.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0cc98d3ce68047f873e119bccaabdbee>
delModel :: Context -> Model -> IO ()
delModel c m = z3_del_model (unContext c) (unModel m)

showModel :: Context -> Model -> IO String
showModel (Context fpc) (Model pm) = z3_model_to_string fpc pm >>= peekCString

showContext :: Context -> IO String
showContext (Context fpc) = z3_context_to_string fpc >>= peekCString

-- | Check whether the given logical context is consistent or not.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga72055cfbae81bd174abed32a83e50b03>
check :: Context -> IO Result
check ctx = toResult <$> z3_check (unContext ctx)

-- TODO Constraints: Z3_check_assumptions
-- TODO Constraints: Z3_get_implied_equalities

-- TODO From section 'Constraints' on.


---------------------------------------------------------------------
-- * Parameters

mkParams :: Context -> IO Params
mkParams c = Params <$> z3_mk_params (unContext c)

paramsSetBool :: Context -> Params -> Symbol -> Bool -> IO ()
paramsSetBool c params sym b =
  z3_params_set_bool (unContext c) (unParams params) (unSymbol sym) (unBool b)

paramsSetUInt :: Context -> Params -> Symbol -> Int -> IO ()
paramsSetUInt c params sym v =
  z3_params_set_uint (unContext c) (unParams params) (unSymbol sym)
                     (fromIntegral v)

paramsSetDouble :: Context -> Params -> Symbol -> Double -> IO ()
paramsSetDouble c params sym v =
  z3_params_set_double (unContext c) (unParams params) (unSymbol sym)
                       (realToFrac v)

paramsSetSymbol :: Context -> Params -> Symbol -> Symbol -> IO ()
paramsSetSymbol c params sym v =
  z3_params_set_symbol (unContext c) (unParams params) (unSymbol sym)
                       (unSymbol v)

paramsToString :: Context -> Params -> IO String
paramsToString (Context c) (Params params) =
  z3_params_to_string c params >>= peekCString


---------------------------------------------------------------------
-- * Solvers

mkSolver :: Context -> IO Solver
mkSolver c = Solver <$> z3_mk_solver (unContext c)

mkSimpleSolver :: Context -> IO Solver
mkSimpleSolver c = Solver <$> z3_mk_simple_solver (unContext c)

mkSolverForLogic :: Context -> String -> IO Solver
mkSolverForLogic c str =
  do sym <- mkStringSymbol c str
     Solver <$> z3_mk_solver_for_logic (unContext c) (unSymbol sym)

solverSetParams :: Context -> Solver -> Params -> IO ()
solverSetParams c solver params =
  z3_solver_set_params (unContext c) (unSolver solver) (unParams params)

solverPush :: Context -> Solver -> IO ()
solverPush c solver = z3_solver_push (unContext c) (unSolver solver)

solverPop :: Context -> Solver -> Int -> IO ()
solverPop c solver i =
  z3_solver_pop (unContext c) (unSolver solver) (fromIntegral i)

solverReset :: Context -> Solver -> IO ()
solverReset c solver = z3_solver_reset (unContext c) (unSolver solver)

solverAssertCnstr :: Context -> Solver -> AST -> IO ()
solverAssertCnstr c solver ast =
  z3_solver_assert (unContext c) (unSolver solver) (unAST ast)

solverAssertAndTrack :: Context -> Solver -> AST -> AST -> IO ()
solverAssertAndTrack c solver constraint var =
  z3_solver_assert_and_track (unContext c) (unSolver solver)
                             (unAST constraint) (unAST var)

solverCheck :: Context -> Solver -> IO Result
solverCheck c solver =
  toResult <$> z3_solver_check (unContext c) (unSolver solver)

solverCheckAndGetModel :: Context -> Solver -> IO (Result, Maybe Model)
solverCheckAndGetModel (Context c) (Solver s) =
  do res <- toResult <$> z3_solver_check c s
     mmodel <- case res of
                 Sat -> (Just . Model) <$> z3_solver_get_model c s
                 _ -> return Nothing
     return (res, mmodel)

solverGetReasonUnknown :: Context -> Solver -> IO String
solverGetReasonUnknown c solver =
  z3_solver_get_reason_unknown (unContext c) (unSolver solver) >>= peekCString

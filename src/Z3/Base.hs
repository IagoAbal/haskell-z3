{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE TupleSections              #-}

-- |
-- Module    : Z3.Base
-- Copyright : (c) Iago Abal, 2012-2014
--             (c) David Castro, 2012-2013
-- License   : BSD3
-- Maintainer: Iago Abal <mail@iagoabal.eu>,
--             David Castro <david.castro.dcp@gmail.com>
--
-- Low-level bindings to Z3 API.
--
-- There is (mostly) a one-to-one correspondence with Z3 C API, thus see
-- <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html>
-- for further details.
--
-- Note that these bindings still focus on the old Z3 3.x API, and will not
-- handle reference counting for you if you decide to use Z3 4.x API functions.
-- Mixing both API is known to cause some segmentation faults!
--
-- Supporting Z3 4.x API (and removing support for 3.x) is planned for
-- the 0.4 version of these bindings.

{- HACKING

Add here the IO-based wrapper for a new Z3 API function:

* Take a look to a few others API functions and follow the same coding style.
    * 2-space wide indentation, no tabs.
    * No trailing spaces, please.
    * ...
* Place the API function in the right section, according to the Z3's API documentation.
* Annotate the API function with a short but concise haddock comment.
    * Look at the Z3 API documentation for inspiration.
* Add the API function to the module export list, (only) if needed.

This should be straightforward for most cases using [MARSHALLING HELPERS].
-}

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
  , FuncInterp
  , FuncEntry
  , Params
  , Solver
  , ASTKind(..)

  -- ** Satisfiability result
  , Result(..)

  -- * Configuration
  , mkConfig
  , delConfig
  , withConfig
  , setParamValue

  -- * Context
  , mkContext
  , delContext
  , withContext
  , contextToString
  , showContext

  -- * Symbols
  , mkIntSymbol
  , mkStringSymbol

  -- * Sorts
  , mkUninterpretedSort
  , mkBoolSort
  , mkIntSort
  , mkRealSort
  , mkBvSort
  , mkArraySort
  , mkTupleSort

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

  -- * Arrays
  , mkSelect
  , mkStore
  , mkConstArray
  , mkMap
  , mkArrayDefault

  -- * Numerals
  , mkNumeral
  , mkInt
  , mkReal

  -- * Quantifiers
  , mkPattern
  , mkBound
  , mkForall
  , mkExists
  , mkForallConst
  , mkExistsConst

  -- * Accessors
  , getAstKind
  , getBvSortSize
  , getSort
  , getBool
  , getInt
  , getReal
  , toApp

  -- * Models
  , FuncModel (..)
  , eval
  , evalT
  , evalFunc
  , evalArray
  , getFuncInterp
  , isAsArray
  , getAsArrayFuncDecl
  , funcInterpGetNumEntries
  , funcInterpGetEntry
  , funcInterpGetElse
  , funcInterpGetArity
  , funcEntryGetValue
  , funcEntryGetNumArgs
  , funcEntryGetArg
  , modelToString
  , showModel

  -- * Constraints
  , assertCnstr
  , check
  , checkAndGetModel
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
  -- /EXPERIMENTAL/ support for solvers,
  -- very fragile and likely to cause crashes!!!
  --
  -- Yet, there are people using it, so you may want to give it a try.
  , Logic(..)
  , mkSolver
  , mkSimpleSolver
  , mkSolverForLogic
  , solverSetParams
  , solverPush
  , solverPop
  , solverReset
  , solverGetNumScopes
  , solverAssertCnstr
  , solverAssertAndTrack
  , solverCheck
  , solverGetModel
  , solverCheckAndGetModel
  , solverGetReasonUnknown
  , solverToString

  -- * String Conversion
  , ASTPrintMode(..)
  , setASTPrintMode
  , astToString
  , patternToString
  , sortToString
  , funcDeclToString
  , benchmarkToSMTLibString

  -- * Miscellaneous
  , Version(..)
  , getVersion

  -- * Error Handling
  , Z3Error(..)
  , Z3ErrorCode(..)
  ) where

import Z3.Base.C

import Control.Applicative ( (<$>), (<*) )
import Control.Exception ( Exception, bracket, throw )
import Control.Monad ( when )
import Data.Int
import Data.Ratio ( Ratio, numerator, denominator, (%) )
import Data.Traversable ( Traversable )
import qualified Data.Traversable as T
import Data.Typeable ( Typeable )
import Data.Word
import Foreign hiding ( toBool, newForeignPtr )
import Foreign.C
  ( CDouble, CInt, CUInt, CLLong, CULLong, CString
  , peekCString
  , withCString )
import Foreign.Concurrent

---------------------------------------------------------------------
-- Types

-- | A Z3 /configuration object/.
newtype Config = Config { unConfig :: Ptr Z3_config }
    deriving Eq

-- | A Z3 /logical context/.
newtype Context = Context { unContext :: Ptr Z3_context }
    deriving Eq

-- | A Z3 /symbol/.
--
-- Used to name types, constants and functions.
newtype Symbol = Symbol { unSymbol :: Ptr Z3_symbol }
    deriving (Eq, Ord, Show, Storable)

-- | A Z3 /AST node/.
--
-- This is the data-structure used in Z3 to represent terms, formulas and types.
newtype AST = AST { unAST :: ForeignPtr Z3_ast }
    deriving (Eq, Ord, Show, Typeable)

-- | A kind of AST representing /types/.
newtype Sort = Sort { unSort :: Ptr Z3_sort }
    deriving (Eq, Ord, Show, Storable)

-- | A kind of AST representing function symbols.
newtype FuncDecl = FuncDecl { unFuncDecl :: Ptr Z3_func_decl }
    deriving (Eq, Ord, Show, Storable, Typeable)

-- | A kind of AST representing constant and function declarations.
newtype App = App { unApp :: Ptr Z3_app }
    deriving (Eq, Ord, Show, Storable)

-- | A kind of AST representing pattern and multi-patterns to
-- guide quantifier instantiation.
newtype Pattern = Pattern { unPattern :: Ptr Z3_pattern }
    deriving (Eq, Ord, Show, Storable)

-- | A model for the constraints asserted into the logical context.
newtype Model = Model { unModel :: Ptr Z3_model }
    deriving Eq

-- | An interpretation of a function in a model.
newtype FuncInterp = FuncInterp { unFuncInterp :: ForeignPtr Z3_func_interp }
    deriving Eq

-- | Representation of the value of a 'Z3_func_interp' at a particular point.
newtype FuncEntry = FuncEntry { unFuncEntry :: ForeignPtr Z3_func_entry }
    deriving Eq

-- | A Z3 parameter set.
--
-- Starting at Z3 4.0, parameter sets are used to configure many components
-- such as: simplifiers, tactics, solvers, etc.
newtype Params = Params { unParams :: ForeignPtr Z3_params }
    deriving Eq

-- | A Z3 solver engine.
--
-- A(n) (incremental) solver, possibly specialized by a particular tactic
-- or logic.
newtype Solver = Solver { unSolver :: ForeignPtr Z3_solver }
    deriving Eq

-- | Result of a satisfiability check.
--
-- This corresponds to the /z3_lbool/ type in the C API.
data Result
    = Sat
    | Unsat
    | Undef
    deriving (Eq, Ord, Read, Show)

-- | Different kinds of Z3 AST nodes.
data ASTKind
    = Z3_NUMERAL_AST
    | Z3_APP_AST
    | Z3_VAR_AST
    | Z3_QUANTIFIER_AST
    | Z3_SORT_AST
    | Z3_FUNC_DECL_AST
    | Z3_UNKNOWN_AST

---------------------------------------------------------------------
-- Configuration

-- | Create a configuration.
mkConfig :: IO Config
mkConfig = Config <$> z3_mk_config

-- | Delete a configuration.
delConfig :: Config -> IO ()
delConfig = z3_del_config . unConfig

-- | Run a computation using a temporally created configuration.
--
-- Note that the configuration object can be thrown away once
-- it has been used to create the Z3 'Context'.
withConfig :: (Config -> IO a) -> IO a
withConfig = bracket mkConfig delConfig

-- | Set a configuration parameter.
setParamValue :: Config -> String -> String -> IO ()
setParamValue cfg s1 s2 =
  withCString s1  $ \cs1 ->
    withCString s2  $ \cs2 ->
      z3_set_param_value (unConfig cfg) cs1 cs2

---------------------------------------------------------------------
-- Context

-- | Create a context using the given configuration.
mkContext :: Config -> IO Context
mkContext cfg = do
  ctxPtr <- z3_mk_context_rc (unConfig cfg)
  z3_set_error_handler ctxPtr nullFunPtr
  return $ Context ctxPtr

-- | Delete the given logical context.
delContext :: Context -> IO ()
delContext = z3_del_context . unContext

-- | Run a computation using a temporally created context.
withContext :: Config -> (Context -> IO a) -> IO a
withContext cfg = bracket (mkContext cfg) delContext

-- | Convert the given logical context into a string.
contextToString :: Context -> IO String
contextToString = liftFun0 z3_context_to_string

-- | Alias for 'contextToString'.
showContext :: Context -> IO String
showContext = contextToString
{-# DEPRECATED showContext "Use contextToString instead." #-}

---------------------------------------------------------------------
-- Symbols

-- | Create a Z3 symbol using an integer.
--
-- @mkIntSymbol c i@ /requires/ @0 <= i < 2^30@
mkIntSymbol :: Integral int => Context -> int -> IO Symbol
mkIntSymbol c i
  | 0 <= i && i <= 2^(30::Int)-1
  = marshal z3_mk_int_symbol c $ h2c i
  | otherwise
  = error "Z3.Base.mkIntSymbol: invalid range"

{-# SPECIALIZE mkIntSymbol :: Context -> Int -> IO Symbol #-}
{-# SPECIALIZE mkIntSymbol :: Context -> Integer -> IO Symbol #-}

-- | Create a Z3 symbol using a string.
mkStringSymbol :: Context -> String -> IO Symbol
mkStringSymbol = liftFun1 z3_mk_string_symbol

---------------------------------------------------------------------
-- Sorts

-- | Create a free (uninterpreted) type using the given name (symbol).
--
-- Two free types are considered the same iff the have the same name.
mkUninterpretedSort :: Context -> Symbol -> IO Sort
mkUninterpretedSort = liftFun1 z3_mk_uninterpreted_sort

-- | Create the /boolean/ type.
--
-- This type is used to create propositional variables and predicates.
mkBoolSort :: Context -> IO Sort
mkBoolSort = liftFun0 z3_mk_bool_sort

-- | Create an /integer/ type.
--
-- This is the type of arbitrary precision integers.
-- A machine integer can be represented using bit-vectors, see 'mkBvSort'.
mkIntSort :: Context -> IO Sort
mkIntSort = liftFun0 z3_mk_int_sort

-- | Create a /real/ type.
--
-- This type is not a floating point number.
-- Z3 does not have support for floating point numbers yet.
mkRealSort :: Context -> IO Sort
mkRealSort = liftFun0 z3_mk_real_sort

-- | Create a bit-vector type of the given size.
--
-- This type can also be seen as a machine integer.
--
-- @mkBvSort c sz@ /requires/ @sz >= 0@
mkBvSort :: Context -> Int -> IO Sort
mkBvSort = liftFun1 z3_mk_bv_sort

-- TODO: Z3_mk_finite_domain_sort

-- | Create an array type
--
-- We usually represent the array type as: [domain -> range].
-- Arrays are usually used to model the heap/memory in software verification.
mkArraySort :: Context -> Sort -> Sort -> IO Sort
mkArraySort = liftFun2 z3_mk_array_sort

-- | Create a tuple type
--
-- A tuple with n fields has a constructor and n projections.
-- This function will also declare the constructor and projection functions.
mkTupleSort :: Context                         -- ^ Context
            -> Symbol                          -- ^ Name of the sort
            -> [(Symbol, Sort)]                -- ^ Name and sort of each field
            -> IO (Sort, FuncDecl, [FuncDecl]) -- ^ Resulting sort, and function
                                               -- declarations for the
                                               -- constructor and projections.
mkTupleSort c sym symSorts = checkError c $
  h2c sym $ \symPtr ->
  marshalArrayLen syms $ \ n symsPtr ->
  marshalArray sorts $ \ sortsPtr ->
  alloca $ \ outConstrPtr ->
  allocaArray n $ \ outProjsPtr -> do
    sort <- z3_mk_tuple_sort (unContext c) symPtr
                            (fromIntegral n) symsPtr sortsPtr
                            outConstrPtr outProjsPtr
    outConstr <- peek outConstrPtr
    outProjs  <- peekArray n outProjsPtr
    return (Sort sort, FuncDecl outConstr, map FuncDecl outProjs)
  where (syms, sorts) = unzip symSorts

-- TODO Sorts: from Z3_mk_enumeration_sort on

---------------------------------------------------------------------
-- Constants and Applications

-- | Declare a constant or function.
mkFuncDecl :: Context   -- ^ Logical context.
            -> Symbol   -- ^ Name of the function (or constant).
            -> [Sort]   -- ^ Function domain (empty for constants).
            -> Sort     -- ^ Return sort of the function.
            -> IO FuncDecl
mkFuncDecl ctx smb dom rng =
  marshal z3_mk_func_decl ctx $ \f ->
    h2c smb $ \ptrSym ->
    marshalArrayLen dom $ \domNum domArr ->
    h2c rng $ \ptrRange ->
      f ptrSym domNum domArr ptrRange

-- | Create a constant or function application.
mkApp :: Context -> FuncDecl -> [AST] -> IO AST
mkApp ctx fd args = marshal z3_mk_app ctx $ \f ->
  h2c fd $ \fdPtr ->
  marshalArrayLen args $ \argsNum argsArr ->
    f fdPtr argsNum argsArr

-- | Declare and create a constant.
--
-- This is a shorthand for:
-- @do xd <- mkFunDecl c x [] s; mkApp c xd []@
mkConst :: Context -> Symbol -> Sort -> IO AST
mkConst = liftFun2 z3_mk_const

-- TODO Constants and Applications: Z3_mk_fresh_func_decl
-- TODO Constants and Applications: Z3_mk_fresh_const

-- | Create an AST node representing /true/.
mkTrue :: Context -> IO AST
mkTrue = liftFun0 z3_mk_true

-- | Create an AST node representing /false/.
mkFalse :: Context -> IO AST
mkFalse = liftFun0 z3_mk_false

-- | Create an AST node representing /l = r/.
mkEq :: Context -> AST -> AST -> IO AST
mkEq = liftFun2 z3_mk_eq

-- | The distinct construct is used for declaring the arguments pairwise
-- distinct.
--
-- That is, @and [ args!!i /= args!!j | i <- [0..length(args)-1], j <- [i+1..length(args)-1] ]@
mkDistinct :: Context -> [AST] -> IO AST
mkDistinct =
  liftAstN "Z3.Base.mkDistinct: empty list of expressions" z3_mk_distinct

-- | Create an AST node representing /not(a)/.
mkNot :: Context -> AST -> IO AST
mkNot = liftFun1 z3_mk_not

-- | Create an AST node representing an if-then-else: /ite(t1, t2, t3)/.
mkIte :: Context -> AST -> AST -> AST -> IO AST
mkIte = liftFun3 z3_mk_ite

-- | Create an AST node representing /t1 iff t2/.
mkIff :: Context -> AST -> AST -> IO AST
mkIff = liftFun2 z3_mk_iff

-- | Create an AST node representing /t1 implies t2/.
mkImplies :: Context -> AST -> AST -> IO AST
mkImplies = liftFun2 z3_mk_implies

-- | Create an AST node representing /t1 xor t2/.
mkXor :: Context -> AST -> AST -> IO AST
mkXor = liftFun2 z3_mk_xor

-- | Create an AST node representing args[0] and ... and args[num_args-1].
mkAnd :: Context -> [AST] -> IO AST
mkAnd = liftAstN "Z3.Base.mkAnd: empty list of expressions" z3_mk_and

-- | Create an AST node representing args[0] or ... or args[num_args-1].
mkOr :: Context -> [AST] -> IO AST
mkOr = liftAstN "Z3.Base.mkOr: empty list of expressions" z3_mk_or

-- | Create an AST node representing args[0] + ... + args[num_args-1].
mkAdd :: Context -> [AST] -> IO AST
mkAdd = liftAstN "Z3.Base.mkAdd: empty list of expressions" z3_mk_add

-- | Create an AST node representing args[0] * ... * args[num_args-1].
mkMul :: Context -> [AST] -> IO AST
mkMul = liftAstN "Z3.Base.mkMul: empty list of expressions" z3_mk_mul

-- | Create an AST node representing args[0] - ... - args[num_args - 1].
mkSub :: Context -> [AST] -> IO AST
mkSub = liftAstN "Z3.Base.mkSub: empty list of expressions" z3_mk_sub

-- | Create an AST node representing -arg.
mkUnaryMinus :: Context -> AST -> IO AST
mkUnaryMinus = liftFun1 z3_mk_unary_minus

-- | Create an AST node representing arg1 div arg2.
mkDiv :: Context -> AST -> AST -> IO AST
mkDiv = liftFun2 z3_mk_div

-- | Create an AST node representing arg1 mod arg2.
mkMod :: Context -> AST -> AST -> IO AST
mkMod = liftFun2 z3_mk_mod

-- | Create an AST node representing arg1 rem arg2.
mkRem :: Context -> AST -> AST -> IO AST
mkRem = liftFun2 z3_mk_rem

-- | Create less than.
mkLt :: Context -> AST -> AST -> IO AST
mkLt = liftFun2 z3_mk_lt

-- | Create less than or equal to.
mkLe :: Context -> AST -> AST -> IO AST
mkLe = liftFun2 z3_mk_le

-- | Create greater than.
mkGt :: Context -> AST -> AST -> IO AST
mkGt = liftFun2 z3_mk_gt

-- | Create greater than or equal to.
mkGe :: Context -> AST -> AST -> IO AST
mkGe = liftFun2 z3_mk_ge

-- | Coerce an integer to a real.
mkInt2Real :: Context -> AST -> IO AST
mkInt2Real = liftFun1 z3_mk_int2real

-- | Coerce a real to an integer.
mkReal2Int :: Context -> AST -> IO AST
mkReal2Int = liftFun1 z3_mk_real2int

-- | Check if a real number is an integer.
mkIsInt :: Context -> AST -> IO AST
mkIsInt = liftFun1 z3_mk_is_int

---------------------------------------------------------------------
-- Bit-vectors

-- | Bitwise negation.
mkBvnot :: Context -> AST -> IO AST
mkBvnot = liftFun1 z3_mk_bvnot

-- | Take conjunction of bits in vector, return vector of length 1.
mkBvredand :: Context -> AST -> IO AST
mkBvredand = liftFun1 z3_mk_bvredand

-- | Take disjunction of bits in vector, return vector of length 1.
mkBvredor :: Context -> AST -> IO AST
mkBvredor = liftFun1 z3_mk_bvredor

-- | Bitwise and.
mkBvand :: Context -> AST -> AST -> IO AST
mkBvand = liftFun2 z3_mk_bvand

-- | Bitwise or.
mkBvor :: Context -> AST -> AST -> IO AST
mkBvor = liftFun2 z3_mk_bvor

-- | Bitwise exclusive-or.
mkBvxor :: Context -> AST -> AST -> IO AST
mkBvxor = liftFun2 z3_mk_bvxor

-- | Bitwise nand.
mkBvnand :: Context -> AST -> AST -> IO AST
mkBvnand = liftFun2 z3_mk_bvnand

-- | Bitwise nor.
mkBvnor :: Context -> AST -> AST -> IO AST
mkBvnor = liftFun2 z3_mk_bvnor

-- | Bitwise xnor.
mkBvxnor :: Context -> AST -> AST -> IO AST
mkBvxnor = liftFun2 z3_mk_bvxnor

-- | Standard two's complement unary minus.
mkBvneg :: Context -> AST -> IO AST
mkBvneg = liftFun1 z3_mk_bvneg

-- | Standard two's complement addition.
mkBvadd :: Context -> AST -> AST -> IO AST
mkBvadd = liftFun2 z3_mk_bvadd

-- | Standard two's complement subtraction.
mkBvsub :: Context -> AST -> AST -> IO AST
mkBvsub = liftFun2 z3_mk_bvsub

-- | Standard two's complement multiplication.
mkBvmul :: Context -> AST -> AST -> IO AST
mkBvmul = liftFun2 z3_mk_bvmul

-- | Unsigned division.
mkBvudiv :: Context -> AST -> AST -> IO AST
mkBvudiv = liftFun2 z3_mk_bvudiv

-- | Two's complement signed division.
mkBvsdiv :: Context -> AST -> AST -> IO AST
mkBvsdiv = liftFun2 z3_mk_bvsdiv

-- | Unsigned remainder.
mkBvurem :: Context -> AST -> AST -> IO AST
mkBvurem = liftFun2 z3_mk_bvurem

-- | Two's complement signed remainder (sign follows dividend).
mkBvsrem :: Context -> AST -> AST -> IO AST
mkBvsrem = liftFun2 z3_mk_bvsrem

-- | Two's complement signed remainder (sign follows divisor).
mkBvsmod :: Context -> AST -> AST -> IO AST
mkBvsmod = liftFun2 z3_mk_bvsmod

-- | Unsigned less than.
mkBvult :: Context -> AST -> AST -> IO AST
mkBvult = liftFun2 z3_mk_bvult

-- | Two's complement signed less than.
mkBvslt :: Context -> AST -> AST -> IO AST
mkBvslt = liftFun2 z3_mk_bvslt

-- | Unsigned less than or equal to.
mkBvule :: Context -> AST -> AST -> IO AST
mkBvule = liftFun2 z3_mk_bvule

-- | Two's complement signed less than or equal to.
mkBvsle :: Context -> AST -> AST -> IO AST
mkBvsle = liftFun2 z3_mk_bvsle

-- | Unsigned greater than or equal to.
mkBvuge :: Context -> AST -> AST -> IO AST
mkBvuge = liftFun2 z3_mk_bvuge

-- | Two's complement signed greater than or equal to.
mkBvsge :: Context -> AST -> AST -> IO AST
mkBvsge = liftFun2 z3_mk_bvsge

-- | Unsigned greater than.
mkBvugt :: Context -> AST -> AST -> IO AST
mkBvugt = liftFun2 z3_mk_bvugt

-- | Two's complement signed greater than.
mkBvsgt :: Context -> AST -> AST -> IO AST
mkBvsgt = liftFun2 z3_mk_bvsgt

-- | Concatenate the given bit-vectors.
mkConcat :: Context -> AST -> AST -> IO AST
mkConcat = liftFun2 z3_mk_concat

-- | Extract the bits high down to low from a bitvector of size m to yield a new
-- bitvector of size /n/, where /n = high - low + 1/.
mkExtract :: Context -> Int -> Int -> AST -> IO AST
mkExtract = liftFun3 z3_mk_extract

-- | Sign-extend of the given bit-vector to the (signed) equivalent bitvector
-- of size /m+i/, where /m/ is the size of the given bit-vector.
mkSignExt :: Context -> Int -> AST -> IO AST
mkSignExt = liftFun2 z3_mk_sign_ext

-- | Extend the given bit-vector with zeros to the (unsigned) equivalent
-- bitvector of size /m+i/, where /m/ is the size of the given bit-vector.
mkZeroExt :: Context -> Int -> AST -> IO AST
mkZeroExt = liftFun2 z3_mk_zero_ext

-- | Repeat the given bit-vector up length /i/.
mkRepeat :: Context -> Int -> AST -> IO AST
mkRepeat = liftFun2 z3_mk_repeat

-- | Shift left.
mkBvshl :: Context -> AST -> AST -> IO AST
mkBvshl = liftFun2 z3_mk_bvshl

-- | Logical shift right.
mkBvlshr :: Context -> AST -> AST -> IO AST
mkBvlshr = liftFun2 z3_mk_bvlshr

-- | Arithmetic shift right.
mkBvashr :: Context -> AST -> AST -> IO AST
mkBvashr = liftFun2 z3_mk_bvashr

-- | Rotate bits of /t1/ to the left /i/ times.
mkRotateLeft :: Context -> Int -> AST -> IO AST
mkRotateLeft = liftFun2 z3_mk_rotate_left

-- | Rotate bits of /t1/ to the right /i/ times.
mkRotateRight :: Context -> Int -> AST -> IO AST
mkRotateRight = liftFun2 z3_mk_rotate_right

-- | Rotate bits of /t1/ to the left /t2/ times.
mkExtRotateLeft :: Context -> AST -> AST -> IO AST
mkExtRotateLeft = liftFun2 z3_mk_ext_rotate_left

-- | Rotate bits of /t1/ to the right /t2/ times.
mkExtRotateRight :: Context -> AST -> AST -> IO AST
mkExtRotateRight = liftFun2 z3_mk_ext_rotate_right

-- | Create an /n/ bit bit-vector from the integer argument /t1/.
mkInt2bv :: Context -> Int -> AST -> IO AST
mkInt2bv = liftFun2 z3_mk_int2bv

-- | Create an integer from the bit-vector argument /t1/.
--
-- If /is_signed/ is false, then the bit-vector /t1/ is treated as unsigned.
-- So the result is non-negative and in the range [0..2^/N/-1],
-- where /N/ are the number of bits in /t1/.
-- If /is_signed/ is true, /t1/ is treated as a signed bit-vector.
mkBv2int :: Context -> AST -> Bool -> IO AST
mkBv2int = liftFun2 z3_mk_bv2int

-- | Create a predicate that checks that the bit-wise addition of /t1/ and /t2/
-- does not overflow.
mkBvaddNoOverflow :: Context -> AST -> AST -> Bool -> IO AST
mkBvaddNoOverflow = liftFun3 z3_mk_bvadd_no_overflow

-- | Create a predicate that checks that the bit-wise signed addition of /t1/
-- and /t2/ does not underflow.
mkBvaddNoUnderflow :: Context -> AST -> AST -> IO AST
mkBvaddNoUnderflow = liftFun2 z3_mk_bvadd_no_underflow

-- | Create a predicate that checks that the bit-wise signed subtraction of /t1/
-- and /t2/ does not overflow.
mkBvsubNoOverflow :: Context -> AST -> AST -> IO AST
mkBvsubNoOverflow = liftFun2 z3_mk_bvsub_no_overflow

-- | Create a predicate that checks that the bit-wise subtraction of /t1/ and
-- /t2/ does not underflow.
mkBvsubNoUnderflow :: Context -> AST -> AST -> IO AST
mkBvsubNoUnderflow = liftFun2 z3_mk_bvsub_no_underflow

-- | Create a predicate that checks that the bit-wise signed division of /t1/
-- and /t2/ does not overflow.
mkBvsdivNoOverflow :: Context -> AST -> AST -> IO AST
mkBvsdivNoOverflow = liftFun2 z3_mk_bvsdiv_no_overflow

-- | Check that bit-wise negation does not overflow when /t1/ is interpreted as
-- a signed bit-vector.
mkBvnegNoOverflow :: Context -> AST -> IO AST
mkBvnegNoOverflow = liftFun1 z3_mk_bvneg_no_overflow

-- | Create a predicate that checks that the bit-wise multiplication of /t1/ and
-- /t2/ does not overflow.
mkBvmulNoOverflow :: Context -> AST -> AST -> Bool -> IO AST
mkBvmulNoOverflow = liftFun3 z3_mk_bvmul_no_overflow

-- | Create a predicate that checks that the bit-wise signed multiplication of
-- /t1/ and /t2/ does not underflow.
mkBvmulNoUnderflow :: Context -> AST -> AST -> IO AST
mkBvmulNoUnderflow = liftFun2 z3_mk_bvmul_no_underflow

---------------------------------------------------------------------
-- Arrays

-- | Array read. The argument a is the array and i is the index of the array
-- that gets read.
mkSelect :: Context
            -> AST      -- ^ Array.
            -> AST      -- ^ Index of the array to read.
            -> IO AST
mkSelect = liftFun2 z3_mk_select

-- | Array update.
--
-- The result of this function is an array that is equal to the input array
-- (with respect to select) on all indices except for i, where it maps to v.
--
-- The semantics of this function is given by the theory of arrays described
-- in the SMT-LIB standard. See <http://smtlib.org> for more details.
mkStore :: Context
          -> AST      -- ^ Array.
          -> AST      -- ^ Index /i/ of the array.
          -> AST      -- ^ New value for /i/.
          -> IO AST
mkStore = liftFun3 z3_mk_store

-- | Create the constant array.
--
-- The resulting term is an array, such that a select on an arbitrary index
-- produces the value /v/.
mkConstArray :: Context
            -> Sort   -- ^ Domain sort of the array.
            -> AST    -- ^ Value /v/ that the array maps to.
            -> IO AST
mkConstArray = liftFun2 z3_mk_const_array

-- | Map a function /f/ on the the argument arrays.
--
-- The /n/ nodes args must be of array sorts [domain -> range_i].
-- The function declaration /f/ must have type range_1 .. range_n -> range.
-- The sort of the result is [domain -> range].
mkMap :: Context
        -> FuncDecl   -- ^ Function /f/.
        -> [AST]      -- ^ List of arrays.
        -> IO AST
mkMap ctx fun args = marshal z3_mk_map ctx $ \f ->
  h2c fun $ \funPtr ->
  marshalArrayLen args $ \argsNum argsArr ->
    f funPtr argsNum argsArr

-- | Access the array default value.
--
-- Produces the default range value, for arrays that can be represented as
-- finite maps with a default range value.
mkArrayDefault :: Context
                -> AST      -- ^ Array.
                -> IO AST
mkArrayDefault = liftFun1 z3_mk_array_default

-- TODO: Sets

---------------------------------------------------------------------
-- Numerals

-- | Create a numeral of a given sort.
mkNumeral :: Context -> String -> Sort -> IO AST
mkNumeral = liftFun2 z3_mk_numeral

-------------------------------------------------
-- Numerals / Integers

-- | Create a numeral of sort /int/.
mkInt :: Integral a => Context -> a -> IO AST
mkInt c n = mkIntSort c >>= mkNumeral c n_str
  where n_str = show $ toInteger n

{-# INLINE mkIntZ3 #-}
mkIntZ3 :: Context -> Int32 -> Sort -> IO AST
mkIntZ3 c n s = marshal z3_mk_int c $ h2c s . withIntegral n

{-# INLINE mkUnsignedIntZ3 #-}
mkUnsignedIntZ3 :: Context -> Word32 -> Sort -> IO AST
mkUnsignedIntZ3 c n s = marshal z3_mk_unsigned_int c $
  h2c s . withIntegral n

{-# INLINE mkInt64Z3 #-}
mkInt64Z3 :: Context -> Int64 -> Sort -> IO AST
mkInt64Z3 = liftFun2 z3_mk_int64

{-# INLINE mkUnsignedInt64Z3 #-}
mkUnsignedInt64Z3 :: Context -> Word64 -> Sort -> IO AST
mkUnsignedInt64Z3 = liftFun2 z3_mk_unsigned_int64

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
mkRealZ3 c r = checkError c $ c2h c =<< z3_mk_real (unContext c) n d
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

-- | Create a pattern for quantifier instantiation.
--
-- Z3 uses pattern matching to instantiate quantifiers.
-- If a pattern is not provided for a quantifier, then Z3 will automatically
-- compute a set of patterns for it. However, for optimal performance,
-- the user should provide the patterns.
--
-- Patterns comprise a list of terms.
-- The list should be non-empty.
-- If the list comprises of more than one term, it is a called a multi-pattern.
--
-- In general, one can pass in a list of (multi-)patterns in the quantifier
-- constructor.
mkPattern :: Context
              -> [AST]        -- ^ Terms.
              -> IO Pattern
mkPattern _ [] = error "Z3.Base.mkPattern: empty list of expressions"
mkPattern c es = marshal z3_mk_pattern c $ marshalArrayLen es

-- | Create a bound variable.
--
-- Bound variables are indexed by de-Bruijn indices.
--
-- See <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1d4da8849fca699b345322f8ee1fa31e>
mkBound :: Context
            -> Int    -- ^ de-Bruijn index.
            -> Sort
            -> IO AST
mkBound c i s
  | i >= 0    = liftFun2 z3_mk_bound c i s
  | otherwise = error "Z3.Base.mkBound: negative de-Bruijn index"

type MkZ3Quantifier = Ptr Z3_context -> CUInt
                      -> CUInt -> Ptr (Ptr Z3_pattern)
                      -> CUInt -> Ptr (Ptr Z3_sort) -> Ptr (Ptr Z3_symbol)
                      -> Ptr Z3_ast
                      -> IO (Ptr Z3_ast)

-- TODO: Allow the user to specify the quantifier weight!
marshalMkQ :: MkZ3Quantifier
          -> Context
          -> [Pattern]
          -> [Symbol]
          -> [Sort]
          -> AST
          -> IO AST
marshalMkQ z3_mk_Q ctx pats x s body = marshal z3_mk_Q ctx $ \f ->
  marshalArrayLen pats $ \n patsArr ->
  marshalArray x $ \xArr ->
  marshalArray s $ \sArr ->
  h2c body $ \bodyPtr ->
    f 0 n patsArr len sArr xArr bodyPtr
  where len
          | l == 0        = error "Z3.Base.mkQuantifier:\
              \ quantifier with 0 bound variables"
          | l /= length x = error "Z3.Base.mkQuantifier:\
              \ different number of symbols and sorts"
          | otherwise     = fromIntegral l
          where l = length s

-- | Create a forall formula.
--
-- The bound variables are de-Bruijn indices created using 'mkBound'.
--
-- Z3 applies the convention that the last element in /xs/ refers to the
-- variable with index 0, the second to last element of /xs/ refers to the
-- variable with index 1, etc.
mkForall :: Context
          -> [Pattern]  -- ^ Instantiation patterns (see 'mkPattern').
          -> [Symbol]   -- ^ Bound (quantified) variables /xs/.
          -> [Sort]     -- ^ Sorts of the bound variables.
          -> AST        -- ^ Body of the quantifier.
          -> IO AST
mkForall = marshalMkQ z3_mk_forall

-- | Create an exists formula.
--
-- Similar to 'mkForall'.
mkExists :: Context -> [Pattern] -> [Symbol] -> [Sort] -> AST -> IO AST
mkExists = marshalMkQ z3_mk_exists

type MkZ3QuantifierConst = Ptr Z3_context
                           -> CUInt
                           -> CUInt
                           -> Ptr (Ptr Z3_app)
                           -> CUInt
                           -> Ptr (Ptr Z3_pattern)
                           -> Ptr Z3_ast
                           -> IO (Ptr Z3_ast)

marshalMkQConst :: MkZ3QuantifierConst
                  -> Context
                  -> [Pattern]
                  -> [App]
                  -> AST
                -> IO AST
marshalMkQConst z3_mk_Q_const ctx pats apps body =
  marshal z3_mk_Q_const ctx $ \f ->
    marshalArrayLen pats $ \patsNum patsArr ->
    marshalArray    apps $ \appsArr ->
    h2c body $ \bodyPtr ->
      f 0 len appsArr patsNum patsArr bodyPtr
  where len
          | l == 0        = error "Z3.Base.mkQuantifierConst:\
              \ quantifier with 0 bound variables"
          | otherwise     = fromIntegral l
          where l = length apps
-- TODO: Allow the user to specify the quantifier weight!

-- | Create a universal quantifier using a list of constants that will form the
-- set of bound variables.
mkForallConst :: Context
              -> [Pattern] -- ^ Instantiation patterns (see 'mkPattern').
              -> [App]     -- ^ Constants to be abstracted into bound variables.
              -> AST       -- ^ Quantifier body.
              -> IO AST
mkForallConst = marshalMkQConst z3_mk_forall_const

-- | Create a existential quantifier using a list of constants that will form
-- the set of bound variables.
mkExistsConst :: Context
              -> [Pattern] -- ^ Instantiation patterns (see 'mkPattern').
              -> [App]     -- ^ Constants to be abstracted into bound variables.
              -> AST       -- ^ Quantifier body.
              -> IO AST
mkExistsConst = marshalMkQConst z3_mk_exists_const

---------------------------------------------------------------------
-- Accessors

-- | Return the kind of the given AST.
getAstKind :: Context -> AST -> IO ASTKind
getAstKind ctx ast = toAstKind <$> liftFun1 z3_get_ast_kind ctx ast

toAstKind :: Z3_ast_kind -> ASTKind
toAstKind k
  | k == z3_numeral_ast       = Z3_NUMERAL_AST
  | k == z3_app_ast           = Z3_APP_AST
  | k == z3_var_ast           = Z3_VAR_AST
  | k == z3_quantifier_ast    = Z3_QUANTIFIER_AST
  | k == z3_sort_ast          = Z3_SORT_AST
  | k == z3_func_decl_ast     = Z3_FUNC_DECL_AST
  | k == z3_unknown_ast       = Z3_UNKNOWN_AST
  | otherwise                 = error "Z3.Base.toAstKind: unknown `Z3_ast_kind'"

-- | Return the size of the given bit-vector sort.
getBvSortSize :: Context -> Sort -> IO Int
getBvSortSize = liftFun1 z3_get_bv_sort_size

-- | Return the sort of an AST node.
getSort :: Context -> AST -> IO Sort
getSort = liftFun1 z3_get_sort

-- | Cast a 'Z3_lbool' from Z3.Base.C to @Bool@.
castLBool :: Z3_lbool -> Maybe Bool
castLBool lb
    | lb == z3_l_true  = Just True
    | lb == z3_l_false = Just False
    | lb == z3_l_undef = Nothing
    | otherwise        = error "Z3.Base.castLBool: illegal `Z3_lbool' value"

-- | Return Z3_L_TRUE if a is true, Z3_L_FALSE if it is false, and Z3_L_UNDEF
-- otherwise.
getBool :: Context -> AST -> IO (Maybe Bool)
getBool c a = checkError c $ h2c a $ \astPtr ->
  castLBool <$> z3_get_bool_value (unContext c) astPtr

-- | Return numeral value, as a string of a numeric constant term.
getNumeralString :: Context -> AST -> IO String
getNumeralString = liftFun1 z3_get_numeral_string

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


-- | Convert an ast into an APP_AST. This is just type casting.
toApp :: Context -> AST -> IO App
toApp = liftFun1 z3_to_app

-- TODO Modifiers

---------------------------------------------------------------------
-- Models

-- | Evaluate an AST node in the given model.
--
-- The evaluation may fail for the following reasons:
--
--     * /t/ contains a quantifier.
--     * the model m is partial.
--     * /t/ is type incorrect.
eval :: Context
        -> Model  -- ^ Model /m/.
        -> AST    -- ^ Expression to evaluate /t/.
        -> IO (Maybe AST)
eval ctx m a =
  alloca $ \aptr2 ->
    h2c a $ \astPtr ->
    checkError ctx $ z3_eval ctxPtr (unModel m) astPtr aptr2 >>= peekAST aptr2 . toBool
  where peekAST :: Ptr (Ptr Z3_ast) -> Bool -> IO (Maybe AST)
        peekAST _p False = return Nothing
        peekAST  p True  = fmap Just . c2h ctx =<< peek p

        ctxPtr = unContext ctx

-- | Evaluate a /collection/ of AST nodes in the given model.
evalT :: Traversable t => Context -> Model -> t AST -> IO (Maybe (t AST))
evalT c m = fmap T.sequence . T.mapM (eval c m)

-- | The interpretation of a function.
data FuncModel = FuncModel
    { interpMap :: [([AST], AST)]
      -- ^ Mapping from arguments to values.
    , interpElse :: AST
      -- ^ Default value.
    }

-- | Evaluate a function declaration to a list of argument/value pairs.
evalFunc :: Context -> Model -> FuncDecl -> IO (Maybe FuncModel)
evalFunc ctx m fDecl =
    do interpMb <- getFuncInterp ctx m fDecl
       case interpMb of
         Nothing -> return Nothing
         Just interp ->
             do funcMap  <- getMapFromInterp ctx interp
                elsePart <- funcInterpGetElse ctx interp
                return (Just $ FuncModel funcMap elsePart)

-- | Evaluate an array as a function, if possible.
evalArray :: Context -> Model -> AST -> IO (Maybe FuncModel)
evalArray ctx model array =
    do -- The array must first be evaluated normally, to get it into
       -- 'as-array' form, which is required to acquire the FuncDecl.
       evaldArrayMb <- eval ctx model array
       case evaldArrayMb of
         Nothing -> return Nothing
         Just evaldArray ->
             do canConvert <- isAsArray ctx evaldArray
                if canConvert
                  then
                    do arrayDecl <- getAsArrayFuncDecl ctx evaldArray
                       evalFunc ctx model arrayDecl
                  else return Nothing


-- | Return the function declaration f associated with a (_ as_array f) node.
getAsArrayFuncDecl :: Context -> AST -> IO FuncDecl
getAsArrayFuncDecl = liftFun1 z3_get_as_array_func_decl

-- | The (_ as-array f) AST node is a construct for assigning interpretations
-- for arrays in Z3. It is the array such that forall indices i we have that
-- (select (_ as-array f) i) is equal to (f i). This procedure returns Z3_TRUE
-- if the a is an as-array AST node.
isAsArray :: Context -> AST -> IO Bool
isAsArray = liftFun1 z3_is_as_array


getMapFromInterp :: Context -> FuncInterp -> IO [([AST], AST)]
getMapFromInterp ctx interp =
    do n <- funcInterpGetNumEntries ctx interp
       entries <- mapM (funcInterpGetEntry ctx interp) [0..n-1]
       mapM (getEntry ctx) entries

getEntry :: Context -> FuncEntry -> IO ([AST], AST)
getEntry ctx entry =
    do val <- funcEntryGetValue ctx entry
       args <- getEntryArgs ctx entry
       return (args, val)

getEntryArgs :: Context -> FuncEntry -> IO [AST]
getEntryArgs ctx entry =
    do n <- funcEntryGetNumArgs ctx entry
       mapM (funcEntryGetArg ctx entry) [0..n-1]

-- | Return the interpretation of the function f in the model m.
-- Return NULL, if the model does not assign an interpretation for f.
-- That should be interpreted as: the f does not matter.
getFuncInterp :: Context -> Model -> FuncDecl -> IO (Maybe FuncInterp)
getFuncInterp ctx m fd = marshal z3_model_get_func_interp ctx $ \f ->
  h2c m $ \mPtr ->
  h2c fd $ \fdPtr ->
    f mPtr fdPtr

-- | Return the number of entries in the given function interpretation.
funcInterpGetNumEntries :: Context -> FuncInterp -> IO Int
funcInterpGetNumEntries = liftFun1 z3_func_interp_get_num_entries

-- | Return a _point_ of the given function intepretation.
-- It represents the value of f in a particular point.
funcInterpGetEntry :: Context -> FuncInterp -> Int -> IO FuncEntry
funcInterpGetEntry = liftFun2 z3_func_interp_get_entry

-- | Return the 'else' value of the given function interpretation.
funcInterpGetElse :: Context -> FuncInterp -> IO AST
funcInterpGetElse = liftFun1 z3_func_interp_get_else

-- | Return the arity (number of arguments) of the given function
-- interpretation.
funcInterpGetArity :: Context -> FuncInterp -> IO Int
funcInterpGetArity = liftFun1 z3_func_interp_get_arity

-- | Return the value of this point.
funcEntryGetValue :: Context -> FuncEntry -> IO AST
funcEntryGetValue = liftFun1 z3_func_entry_get_value

-- | Return the number of arguments in a Z3_func_entry object.
funcEntryGetNumArgs :: Context -> FuncEntry -> IO Int
funcEntryGetNumArgs = liftFun1 z3_func_entry_get_num_args

-- | Return an argument of a Z3_func_entry object.
funcEntryGetArg :: Context -> FuncEntry -> Int -> IO AST
funcEntryGetArg = liftFun2 z3_func_entry_get_arg

-- | Convert the given model into a string.
modelToString :: Context -> Model -> IO String
modelToString = liftFun1 z3_model_to_string

-- | Alias for 'modelToString'.
showModel :: Context -> Model -> IO String
showModel = modelToString
{-# DEPRECATED showModel "Use modelToString instead." #-}

---------------------------------------------------------------------
-- Constraints

-- | Create a backtracking point.
push :: Context -> IO ()
push = liftFun0 z3_push

-- | Backtrack /n/ backtracking points.
pop :: Context -> Int -> IO ()
pop = liftFun1 z3_pop

-- TODO Constraints: Z3_get_num_scopes

-- TODO Constraints: Z3_persist_ast

-- | Assert a constraing into the logical context.
assertCnstr :: Context -> AST -> IO ()
assertCnstr = liftFun1 z3_assert_cnstr

-- | Check whether the given logical context is consistent or not.
getModel :: Context -> IO (Result, Maybe Model)
getModel c =
  alloca $ \mptr ->
    checkError c (z3_check_and_get_model (unContext c) mptr) >>= \lb ->
      (toResult lb,) <$> peekModel mptr
  where peekModel :: Ptr (Ptr Z3_model) -> IO (Maybe Model)
        peekModel p | p == nullPtr = return Nothing
                    | otherwise    = mkModel <$> peek p
        mkModel :: Ptr Z3_model -> Maybe Model
        mkModel = fmap Model . ptrToMaybe
{-# DEPRECATED getModel "Use checkAndGetModel instead." #-}

-- | Check whether the given logical context is consistent or not.
--
-- If the logical context is not unsatisfiable  and model construction is
-- enabled (via 'mkConfig'), then a model is returned. The caller is
-- responsible for deleting the model using the function 'delModel'.
checkAndGetModel :: Context -> IO (Result, Maybe Model)
checkAndGetModel = getModel

-- | Delete a model object.
delModel :: Context -> Model -> IO ()
delModel = liftFun1 z3_del_model

-- | Check whether the given logical context is consistent or not.
check :: Context -> IO Result
check ctx = checkError ctx $ toResult <$> z3_check (unContext ctx)

-- TODO Constraints: Z3_check_assumptions
-- TODO Constraints: Z3_get_implied_equalities

-- TODO From section 'Constraints' on.

---------------------------------------------------------------------
-- Parameters

{-# WARNING mkParams
          , paramsSetBool
          , paramsSetUInt
          , paramsSetDouble
          , paramsSetSymbol
          , paramsToString
          "New Z3 API support is still incomplete and fragile: \
          \you may experience segmentation faults!"
  #-}

mkParams :: Context -> IO Params
mkParams = liftFun0 z3_mk_params

paramsSetBool :: Context -> Params -> Symbol -> Bool -> IO ()
paramsSetBool = liftFun3 z3_params_set_bool

paramsSetUInt :: Context -> Params -> Symbol -> Int -> IO ()
paramsSetUInt = liftFun3 z3_params_set_uint

paramsSetDouble :: Context -> Params -> Symbol -> Double -> IO ()
paramsSetDouble = liftFun3 z3_params_set_double

paramsSetSymbol :: Context -> Params -> Symbol -> Symbol -> IO ()
paramsSetSymbol = liftFun3 z3_params_set_symbol

paramsToString :: Context -> Params -> IO String
paramsToString = liftFun1 z3_params_to_string

---------------------------------------------------------------------
-- Solvers

{-# WARNING Logic
          , mkSolver
          , mkSimpleSolver
          , mkSolverForLogic
          , solverSetParams
          , solverPush
          , solverPop
          , solverReset
          , solverGetNumScopes
          , solverAssertCnstr
          , solverAssertAndTrack
          , solverCheck
          , solverCheckAndGetModel
          , solverGetReasonUnknown
          "New Z3 API support is still incomplete and fragile: \
          \you may experience segmentation faults!"
  #-}

-- | Solvers available in Z3.
--
-- These are described at <http://smtlib.cs.uiowa.edu/logics.html>
--
-- /WARNING/: Support for solvers is fragile, you may experience segmentation
-- faults!
data Logic
  = AUFLIA
    -- ^ Closed formulas over the theory of linear integer arithmetic
    -- and arrays extended with free sort and function symbols but
    -- restricted to arrays with integer indices and values.

  | AUFLIRA
    -- ^ Closed linear formulas with free sort and function symbols over
    -- one- and two-dimentional arrays of integer index and real
    -- value.

  | AUFNIRA
    -- ^ Closed formulas with free function and predicate symbols over a
    -- theory of arrays of arrays of integer index and real value.

  | LRA
    -- ^ Closed linear formulas in linear real arithmetic.

  | QF_ABV
    -- ^ Closed quantifier-free formulas over the theory of bitvectors
    -- and bitvector arrays.

  | QF_AUFBV
    -- ^ Closed quantifier-free formulas over the theory of bitvectors
    -- and bitvector arrays extended with free sort and function
    -- symbols.

  | QF_AUFLIA
    -- ^ Closed quantifier-free linear formulas over the theory of
    -- integer arrays extended with free sort and function symbols.

  | QF_AX
    -- ^ Closed quantifier-free formulas over the theory of arrays with
    -- extensionality.

  | QF_BV
    -- ^ Closed quantifier-free formulas over the theory of fixed-size
    -- bitvectors.

  | QF_IDL
    -- ^ Difference Logic over the integers. In essence, Boolean
    -- combinations of inequations of the form x - y < b where x and y
    -- are integer variables and b is an integer constant.

  | QF_LIA
    -- ^ Unquantified linear integer arithmetic. In essence, Boolean
    -- combinations of inequations between linear polynomials over
    -- integer variables.

  | QF_LRA
    -- ^ Unquantified linear real arithmetic. In essence, Boolean
    -- combinations of inequations between linear polynomials over
    -- real variables.

  | QF_NIA
    -- ^ Quantifier-free integer arithmetic.

  | QF_NRA
    -- ^ Quantifier-free real arithmetic.

  | QF_RDL
    -- ^ Difference Logic over the reals. In essence, Boolean
    -- combinations of inequations of the form x - y < b where x and y
    -- are real variables and b is a rational constant.

  | QF_UF
    -- ^ Unquantified formulas built over a signature of uninterpreted
    -- (i.e., free) sort and function symbols.

  | QF_UFBV
    -- ^ Unquantified formulas over bitvectors with uninterpreted sort
    -- function and symbols.

  | QF_UFIDL
    -- ^ Difference Logic over the integers (in essence) but with
    -- uninterpreted sort and function symbols.

  | QF_UFLIA
    -- ^ Unquantified linear integer arithmetic with uninterpreted sort
    -- and function symbols.

  | QF_UFLRA
    -- ^ Unquantified linear real arithmetic with uninterpreted sort and
    -- function symbols.

  | QF_UFNRA
    -- ^ Unquantified non-linear real arithmetic with uninterpreted sort
    -- and function symbols.

  | UFLRA
    -- ^ Linear real arithmetic with uninterpreted sort and function
    -- symbols.

  | UFNIA
    -- ^ Non-linear integer arithmetic with uninterpreted sort and
    -- function symbols.

instance Show Logic where
  show AUFLIA    = "AUFLIA"
  show AUFLIRA   = "AUFLIRA"
  show AUFNIRA   = "AUFNIRA"
  show LRA       = "LRA"
  show QF_ABV    = "QF_ABV"
  show QF_AUFBV  = "QF_AUFBV"
  show QF_AUFLIA = "QF_AUFLIA"
  show QF_AX     = "QF_AX"
  show QF_BV     = "QF_BV"
  show QF_IDL    = "QF_IDL"
  show QF_LIA    = "QF_LIA"
  show QF_LRA    = "QF_LRA"
  show QF_NIA    = "QF_NIA"
  show QF_NRA    = "QF_NRA"
  show QF_RDL    = "QF_RDL"
  show QF_UF     = "QF_UF"
  show QF_UFBV   = "QF_UFBV"
  show QF_UFIDL  = "QF_UFIDL"
  show QF_UFLIA  = "QF_UFLIA"
  show QF_UFLRA  = "QF_UFLRA"
  show QF_UFNRA  = "QF_UFNRA"
  show UFLRA     = "UFLRA"
  show UFNIA     = "UFNIA"

mkSolver :: Context -> IO Solver
mkSolver = liftFun0 z3_mk_solver

mkSimpleSolver :: Context -> IO Solver
mkSimpleSolver = liftFun0 z3_mk_simple_solver

mkSolverForLogic :: Context -> Logic -> IO Solver
mkSolverForLogic c logic =
  do sym <- mkStringSymbol c (show logic)
     checkError c $
       c2h c =<< z3_mk_solver_for_logic (unContext c) (unSymbol sym)

solverSetParams :: Context -> Solver -> Params -> IO ()
solverSetParams = liftFun2 z3_solver_set_params

solverPush :: Context -> Solver -> IO ()
solverPush = liftFun1 z3_solver_push

solverPop :: Context -> Solver -> Int -> IO ()
solverPop = liftFun2 z3_solver_pop

solverReset :: Context -> Solver -> IO ()
solverReset = liftFun1 z3_solver_reset

-- | Number of backtracking points.
solverGetNumScopes :: Context -> Solver -> IO Int
solverGetNumScopes = liftFun1 z3_solver_get_num_scopes

solverAssertCnstr :: Context -> Solver -> AST -> IO ()
solverAssertCnstr = liftFun2 z3_solver_assert

solverAssertAndTrack :: Context -> Solver -> AST -> AST -> IO ()
solverAssertAndTrack = liftFun3 z3_solver_assert_and_track

-- | Check whether the assertions in a given solver are consistent or not.
solverCheck :: Context -> Solver -> IO Result
solverCheck ctx solver = marshal z3_solver_check ctx $ h2c solver

-- Retrieve the model for the last 'Z3_solver_check'.
--
-- The error handler is invoked if a model is not available because
-- the commands above were not invoked for the given solver,
-- or if the result was 'Unsat'.
solverGetModel :: Context -> Solver -> IO Model
solverGetModel ctx solver = marshal z3_solver_get_model ctx $ \f ->
  h2c solver $ \solverPtr ->
    f solverPtr

solverCheckAndGetModel :: Context -> Solver -> IO (Result, Maybe Model)
solverCheckAndGetModel ctx solver =
  do res <- solverCheck ctx solver
     mbModel <- case res of
                  Unsat -> return Nothing
                  _     -> Just <$> solverGetModel ctx solver
     return (res, mbModel)

solverGetReasonUnknown :: Context -> Solver -> IO String
solverGetReasonUnknown = liftFun1 z3_solver_get_reason_unknown

-- | Convert the given solver into a string.
solverToString :: Context -> Solver -> IO String
solverToString = liftFun1 z3_solver_to_string

---------------------------------------------------------------------
-- String Conversion

-- | Pretty-printing mode for converting ASTs to strings.  The mode
-- can be one of the following:
--
-- * Z3_PRINT_SMTLIB_FULL: Print AST nodes in SMTLIB verbose format.
--
-- * Z3_PRINT_LOW_LEVEL: Print AST nodes using a low-level format.
--
-- * Z3_PRINT_SMTLIB_COMPLIANT: Print AST nodes in SMTLIB 1.x
-- compliant format.
--
-- * Z3_PRINT_SMTLIB2_COMPLIANT: Print AST nodes in SMTLIB 2.x
-- compliant format.
data ASTPrintMode
  = Z3_PRINT_SMTLIB_FULL
  | Z3_PRINT_LOW_LEVEL
  | Z3_PRINT_SMTLIB_COMPLIANT
  | Z3_PRINT_SMTLIB2_COMPLIANT

-- | Set the pretty-printing mode for converting ASTs to strings.
setASTPrintMode :: Context -> ASTPrintMode -> IO ()
setASTPrintMode ctx Z3_PRINT_SMTLIB_FULL =
  checkError ctx $ z3_set_ast_print_mode (unContext ctx) z3_print_smtlib_full
setASTPrintMode ctx Z3_PRINT_LOW_LEVEL =
  checkError ctx $ z3_set_ast_print_mode (unContext ctx) z3_print_low_level
setASTPrintMode ctx Z3_PRINT_SMTLIB_COMPLIANT =
  checkError ctx $ z3_set_ast_print_mode (unContext ctx) z3_print_smtlib_compliant
setASTPrintMode ctx Z3_PRINT_SMTLIB2_COMPLIANT =
  checkError ctx $ z3_set_ast_print_mode (unContext ctx) z3_print_smtlib2_compliant

-- | Convert an AST to a string.
astToString :: Context -> AST -> IO String
astToString = liftFun1 z3_ast_to_string

-- | Convert a pattern to a string.
patternToString :: Context -> Pattern -> IO String
patternToString = liftFun1 z3_pattern_to_string

-- | Convert a sort to a string.
sortToString :: Context -> Sort -> IO String
sortToString = liftFun1 z3_sort_to_string

-- | Convert a FuncDecl to a string.
funcDeclToString :: Context -> FuncDecl -> IO String
funcDeclToString = liftFun1 z3_func_decl_to_string

-- | Convert the given benchmark into SMT-LIB formatted string.
--
-- The output format can be configured via 'setASTPrintMode'.
benchmarkToSMTLibString :: Context
                            -> String   -- ^ name
                            -> String   -- ^ logic
                            -> String   -- ^ status
                            -> String   -- ^ attributes
                            -> [AST]    -- ^ assumptions
                            -> AST      -- ^ formula
                            -> IO String
benchmarkToSMTLibString ctx name logic status attr assump form =
  marshal z3_benchmark_to_smtlib_string ctx $ \f ->
    withCString name $ \namePtr ->
    withCString logic $ \logicPtr ->
    withCString status $ \statusPtr ->
    withCString attr $ \attrPtr ->
    marshalArrayLen assump $ \assumpNum assumpArr ->
    h2c form $ \formPtr ->
      f namePtr logicPtr statusPtr attrPtr assumpNum assumpArr formPtr

---------------------------------------------------------------------
-- Error handling

-- | Z3 exceptions.
data Z3Error = Z3Error
    { errCode :: Z3ErrorCode
    , errMsg  :: String
    }
  deriving Typeable

instance Show Z3Error where
  show (Z3Error _ s) = s

data Z3ErrorCode = SortError | IOB | InvalidArg | ParserError | NoParser
  | InvalidPattern | MemoutFail  | FileAccessError | InternalFatal
  | InvalidUsage   | DecRefError | Z3Exception
  deriving (Show, Typeable)

toZ3Error :: Z3_error_code -> Z3ErrorCode
toZ3Error e
  | e == z3_sort_error        = SortError
  | e == z3_iob               = IOB
  | e == z3_invalid_arg       = InvalidArg
  | e == z3_parser_error      = ParserError
  | e == z3_no_parser         = NoParser
  | e == z3_invalid_pattern   = InvalidPattern
  | e == z3_memout_fail       = MemoutFail
  | e == z3_file_access_error = FileAccessError
  | e == z3_internal_fatal    = InternalFatal
  | e == z3_invalid_usage     = InvalidUsage
  | e == z3_dec_ref_error     = DecRefError
  | e == z3_exception         = Z3Exception
  | otherwise                 = error "Z3.Base.toZ3Error: illegal `Z3_error_code' value"

instance Exception Z3Error

-- | Throws a z3 error
z3Error :: Z3ErrorCode -> String -> IO ()
z3Error cd = throw . Z3Error cd

-- | Throw an exception if a Z3 error happened
checkError :: Context -> IO a -> IO a
checkError c m = m <* (z3_get_error_code (unContext c) >>= throwZ3Exn)
  where throwZ3Exn i = when (i /= z3_ok) $ getErrStr i >>= z3Error (toZ3Error i)
        getErrStr i  = peekCString =<< z3_get_error_msg_ex (unContext c) i

---------------------------------------------------------------------
-- Miscellaneous

data Version
  = Version {
      z3Major    :: !Int
    , z3Minor    :: !Int
    , z3Build    :: !Int
    , z3Revision :: !Int
    }
  deriving (Eq,Ord)

instance Show Version where
  show (Version major minor build _) =
    show major ++ "." ++ show minor ++ "." ++ show build

-- | Return Z3 version number information.
getVersion :: IO Version
getVersion =
  alloca $ \ptrMinor ->
  alloca $ \ptrMajor ->
  alloca $ \ptrBuild ->
  alloca $ \ptrRevision -> do
    z3_get_version ptrMinor ptrMajor ptrBuild ptrRevision
    minor    <- fromIntegral <$> peek ptrMinor
    major    <- fromIntegral <$> peek ptrMajor
    build    <- fromIntegral <$> peek ptrBuild
    revision <- fromIntegral <$> peek ptrRevision
    return $ Version minor major build revision

---------------------------------------------------------------------
-- Marshalling

{- MARSHALLING HELPERS

We try to get rid of most of the marshalling boilerplate which, by the way,
is going to be essential for transitioning to Z3 4 API.

Most API functions can be lifted using 'liftFun'{0-3} helpers. Otherwise try
using 'marshal'. Worst case scenario, write the marshalling code yourself.

-}

withIntegral :: (Integral a, Integral b) => a -> (b -> r) -> r
withIntegral x f = f (fromIntegral x)

marshalArray :: (Marshal h c, Storable c) => [h] -> (Ptr c -> IO a) -> IO a
marshalArray hs f = hs2cs hs $ \cs -> withArray cs f

marshalArrayLen :: (Marshal h c, Storable c, Integral i) =>
    [h] -> (i -> Ptr c -> IO a) -> IO a
marshalArrayLen hs f =
  hs2cs hs $ \cs -> withArrayLen cs $ \n -> f (fromIntegral n)

liftAstN :: String
            -> (Ptr Z3_context -> CUInt -> Ptr (Ptr Z3_ast) -> IO (Ptr Z3_ast))
            -> Context -> [AST] -> IO AST
liftAstN s _ _ [] = error s
liftAstN _ f c es = marshal f c $ marshalArrayLen es
{-# INLINE liftAstN #-}

class Marshal h c where
  c2h :: Context -> c -> IO h
  h2c :: h -> (c -> IO r) -> IO r

hs2cs :: Marshal h c => [h] -> ([c] -> IO r) -> IO r
hs2cs []     f = f []
hs2cs (h:hs) f =
  h2c h $ \c ->
  hs2cs hs $ \cs -> f (c:cs)

instance Marshal h (Ptr x) => Marshal (Maybe h) (Ptr x) where
  c2h c = T.mapM (c2h c) . ptrToMaybe
  h2c Nothing  f = f nullPtr
  h2c (Just x) f = h2c x f

instance Marshal () () where
  c2h _ = return
  h2c x f = f x

instance Marshal Bool Z3_bool where
  c2h _ = return . toBool
  h2c b f = f (unBool b)

instance Marshal Result Z3_lbool where
  c2h _ = return . toResult
  h2c = error "Marshal Result Z3_lbool => h2c not implemented"

instance Integral h => Marshal h CInt where
  c2h _ = return . fromIntegral
  h2c i f = f (fromIntegral i)

instance Integral h => Marshal h CUInt where
  c2h _ = return . fromIntegral
  h2c i f = f (fromIntegral i)

instance Integral h => Marshal h CLLong where
  c2h _ = return . fromIntegral
  h2c i f = f (fromIntegral i)

instance Integral h => Marshal h CULLong where
  c2h _ = return . fromIntegral
  h2c i f = f (fromIntegral i)

instance Marshal Double CDouble where
  c2h _ = return . realToFrac
  h2c d f = f (realToFrac d)

instance Marshal String CString where
  c2h _ = peekCString
  h2c   = withCString

instance Marshal App (Ptr Z3_app) where
  c2h _ = return . App
  h2c a f = f (unApp a)

instance Marshal Params (Ptr Z3_params) where
  c2h ctx prmPtr = do
    z3_params_inc_ref ctxPtr prmPtr
    Params <$> newForeignPtr prmPtr (z3_params_dec_ref ctxPtr prmPtr)
    where ctxPtr = unContext ctx
  h2c prm = withForeignPtr (unParams prm)

instance Marshal Symbol (Ptr Z3_symbol) where
  c2h _ = return . Symbol
  h2c s f = f (unSymbol s)

instance Marshal AST (Ptr Z3_ast) where
  c2h ctx astPtr = do
    z3_inc_ref ctxPtr astPtr
    AST <$> newForeignPtr astPtr (z3_dec_ref ctxPtr astPtr)
    where ctxPtr = unContext ctx
  h2c a f = withForeignPtr (unAST a) f

instance Marshal Sort (Ptr Z3_sort) where
  c2h _ = return . Sort
  h2c a f = f (unSort a)

instance Marshal FuncDecl (Ptr Z3_func_decl) where
  c2h _ = return . FuncDecl
  h2c a f = f (unFuncDecl a)

instance Marshal FuncEntry (Ptr Z3_func_entry) where
  c2h ctx fnePtr = do
    z3_func_entry_inc_ref ctxPtr fnePtr
    FuncEntry <$> newForeignPtr fnePtr (z3_func_entry_dec_ref ctxPtr fnePtr)
    where ctxPtr = unContext ctx
  h2c fne = withForeignPtr (unFuncEntry fne)

instance Marshal FuncInterp (Ptr Z3_func_interp) where
  c2h ctx fniPtr = do
    z3_func_interp_inc_ref ctxPtr fniPtr
    FuncInterp <$> newForeignPtr fniPtr (z3_func_interp_dec_ref ctxPtr fniPtr)
    where ctxPtr = unContext ctx
  h2c fni = withForeignPtr (unFuncInterp fni)

instance Marshal Model (Ptr Z3_model) where
  c2h _ = return . Model
  h2c m f = f (unModel m)

instance Marshal Pattern (Ptr Z3_pattern) where
  c2h _ = return . Pattern
  h2c a f = f (unPattern a)

instance Marshal Solver (Ptr Z3_solver) where
  c2h ctx slvPtr = do
    z3_solver_inc_ref ctxPtr slvPtr
    Solver <$> newForeignPtr slvPtr (z3_solver_dec_ref ctxPtr slvPtr)
    where ctxPtr = unContext ctx
  h2c slv = withForeignPtr (unSolver slv)


marshal :: Marshal rh rc => (Ptr Z3_context -> t) ->
              Context -> (t -> IO rc) -> IO rh
marshal f c cont = checkError c $ cont (f (unContext c)) >>= c2h c

liftFun0 :: Marshal rh rc => (Ptr Z3_context -> IO rc) ->
              Context -> IO rh
liftFun0 f c = checkError c $ c2h c =<< f (unContext c)
{-# INLINE liftFun0 #-}

liftFun1 :: (Marshal ah ac, Marshal rh rc) =>
              (Ptr Z3_context -> ac -> IO rc) ->
              Context -> ah -> IO rh
liftFun1 f c x = checkError c $ h2c x $ \a ->
  c2h c =<< f (unContext c) a
{-# INLINE liftFun1 #-}

liftFun2 :: (Marshal ah ac, Marshal bh bc, Marshal rh rc) =>
              (Ptr Z3_context -> ac -> bc -> IO rc) ->
              Context -> ah -> bh -> IO rh
liftFun2 f c x y = checkError c $ h2c x $ \a -> h2c y $ \b ->
  c2h c =<< f (unContext c) a b
{-# INLINE liftFun2 #-}

liftFun3 :: (Marshal ah ac, Marshal bh bc, Marshal ch cc, Marshal rh rc) =>
              (Ptr Z3_context -> ac -> bc -> cc -> IO rc) ->
              Context -> ah -> bh -> ch -> IO rh
liftFun3 f c x y z = checkError c $
  h2c x $ \x1 -> h2c y $ \y1 -> h2c z $ \z1 ->
    c2h c =<< f (unContext c) x1 y1 z1
{-# INLINE liftFun3 #-}

---------------------------------------------------------------------
-- Utils

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

-- | Wraps a non-null pointer with 'Just', or else returns 'Nothing'.
ptrToMaybe :: Ptr a -> Maybe (Ptr a)
ptrToMaybe ptr | ptr == nullPtr = Nothing
               | otherwise      = Just ptr


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
-- Low-level bindings to Z3 API.

-- TODO Rename showModel and showContext to match Z3 C API names.
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

  -- * Accessors
  , getBvSortSize
  , getSort
  , getBool
  , getInt
  , getReal

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
  , Logic(..)
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

  -- * String Conversion
  , ASTPrintMode(..)
  , setASTPrintMode
  , astToString
  , patternToString
  , sortToString
  , funcDeclToString
  , benchmarkToSMTLibString

  -- * Error Handling
  , Z3Error(..)
  , Z3ErrorCode(..)
  ) where

import Z3.Base.C

import Control.Applicative ( (<$>), (<*) )
import Control.Exception ( Exception, bracket, throw )
import Control.Monad ( when )
import Data.List ( genericLength )
import Data.Int
import Data.Ratio ( Ratio, numerator, denominator, (%) )
import Data.Traversable ( Traversable )
import qualified Data.Traversable as T
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

-- | A interpretation of a function.
newtype FuncInterp = FuncInterp { unFuncInterp :: Ptr Z3_func_interp }
    deriving Eq

--  | An entry in an interpreted function
newtype FuncEntry = FuncEntry { unFuncEntry :: Ptr Z3_func_entry }
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

---------------------------------------------------------------------
-- Context

-- | Create a context using the given configuration.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0bd93cfab4d749dd3e2f2a4416820a46>
mkContext :: Config -> IO Context
mkContext cfg = do
  ctxPtr <- z3_mk_context (unConfig cfg)
  z3_set_error_handler ctxPtr nullFunPtr
  return $ Context ctxPtr

-- | Delete the given logical context.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga556eae80ed43ab13e1e7dc3b38c35200>
delContext :: Context -> IO ()
delContext = z3_del_context . unContext

-- | Run a computation using a temporally created context.
withContext :: Config -> (Context -> IO a) -> IO a
withContext cfg = bracket (mkContext cfg) delContext

-- | Convert the given logical context into a string.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga165e38ddfc928f586cb738cdf6c5f216>
contextToString :: Context -> IO String
contextToString c =
  checkError c $ z3_context_to_string (unContext c) >>= peekCString

-- | Alias for 'contextToString'.
showContext :: Context -> IO String
showContext = contextToString

---------------------------------------------------------------------
-- Symbols

-- | Create a Z3 symbol using an integer.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga3df806baf6124df3e63a58cf23e12411>
mkIntSymbol :: Integral int => Context -> int -> IO Symbol
mkIntSymbol ctx i
  | 0 <= i && i <= 2^(30::Int)-1
  = Symbol <$> z3_mk_int_symbol (unContext ctx) (fromIntegral i)
  | otherwise
  = error "Z3.Base.mkIntSymbol: invalid range"

{-# SPECIALIZE mkIntSymbol :: Context -> Int -> IO Symbol #-}
{-# SPECIALIZE mkIntSymbol :: Context -> Integer -> IO Symbol #-}

-- | Create a Z3 symbol using a string.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gafebb0d3c212927cf7834c3a20a84ecae>
mkStringSymbol :: Context -> String -> IO Symbol
mkStringSymbol ctx s =
  withCString s $ \cs ->
    checkError ctx $
      Symbol <$> z3_mk_string_symbol (unContext ctx) cs

---------------------------------------------------------------------
-- Sorts

-- TODO Sorts: Z3_is_eq_sort

-- | Create a free (uninterpreted) type using the given name (symbol).
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga736e88741af1c178cbebf94c49aa42de>
mkUninterpretedSort :: Context -> Symbol -> IO Sort
mkUninterpretedSort c sy = checkError c $
  Sort <$> z3_mk_uninterpreted_sort (unContext c) (unSymbol sy)

-- | Create the /Boolean/ type.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gacdc73510b69a010b71793d429015f342>
mkBoolSort :: Context -> IO Sort
mkBoolSort c = checkError c $ Sort <$> z3_mk_bool_sort (unContext c)

-- | Create an /integer/ type.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga6cd426ab5748653b77d389fd3eac1015>
mkIntSort :: Context -> IO Sort
mkIntSort c = checkError c $ Sort <$> z3_mk_int_sort (unContext c)

-- | Create a /real/ type.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga40ef93b9738485caed6dc84631c3c1a0>
mkRealSort :: Context -> IO Sort
mkRealSort c = checkError c $ Sort <$> z3_mk_real_sort (unContext c)

-- | Create a bit-vector type of the given size.
--
-- This type can also be seen as a machine integer.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaeed000a1bbb84b6ca6fdaac6cf0c1688>
mkBvSort :: Context -> Int -> IO Sort
mkBvSort c n = checkError c $ Sort <$> z3_mk_bv_sort (unContext c) (fromIntegral n)

-- | Create an array type
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gafe617994cce1b516f46128e448c84445>
--
mkArraySort :: Context -> Sort -> Sort -> IO Sort
mkArraySort c dom rng = checkError c $ Sort <$> z3_mk_array_sort (unContext c) (unSort dom) (unSort rng)

-- | Create a tuple type
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga7156b9c0a76a28fae46c81f8e3cdf0f1>
mkTupleSort :: Context                         -- ^ Context
            -> Symbol                          -- ^ Name of the sort
            -> [(Symbol, Sort)]                -- ^ Name and sort of each field
            -> IO (Sort, FuncDecl, [FuncDecl]) -- ^ Resulting sort, and function
                                               -- declarations for the
                                               -- constructor and projections.
mkTupleSort c sym symSorts = checkError c $
  let (syms, sorts) = unzip symSorts
  in withArrayLen (map unSymbol syms) $ \ n symsPtr ->
     withArray (map unSort sorts) $ \ sortsPtr ->
     alloca $ \ outConstrPtr ->
     allocaArray n $ \ outProjsPtr -> do
       sort <- checkError c $ z3_mk_tuple_sort
                   (unContext c) (unSymbol sym)
                   (fromIntegral n) symsPtr sortsPtr
                   outConstrPtr outProjsPtr
       outConstr <- peek outConstrPtr
       outProjs  <- peekArray n outProjsPtr
       return (Sort sort, FuncDecl outConstr, map FuncDecl outProjs)


-- TODO Sorts: from Z3_mk_array_sort on

---------------------------------------------------------------------
-- Constants and Applications

-- | A Z3 function
mkFuncDecl :: Context -> Symbol -> [Sort] -> Sort -> IO FuncDecl
mkFuncDecl ctx smb dom rng =
  withArray (map unSort dom) $ \c_dom ->
    checkError ctx $
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
    checkError ctx $
      AST <$> z3_mk_app ctxPtr fdPtr numArgs pargs
  where ctxPtr  = unContext ctx
        fdPtr   = unFuncDecl fd
        numArgs = genericLength args

-- | Declare and create a constant.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga093c9703393f33ae282ec5e8729354ef>
mkConst :: Context -> Symbol -> Sort -> IO AST
mkConst c x s = checkError c $ AST <$> z3_mk_const (unContext c) (unSymbol x) (unSort s)

-- TODO Constants and Applications: Z3_mk_fresh_func_decl
-- TODO Constants and Applications: Z3_mk_fresh_const

-- | Create an AST node representing /true/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae898e7380409bbc57b56cc5205ef1db7>
mkTrue :: Context -> IO AST
mkTrue c = checkError c $ AST <$> z3_mk_true (unContext c)

-- | Create an AST node representing /false/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5952ac17671117a02001fed6575c778d>
mkFalse :: Context -> IO AST
mkFalse c = checkError c $ AST <$> z3_mk_false (unContext c)

-- | Create an AST node representing /l = r/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga95a19ce675b70e22bb0401f7137af37c>
mkEq :: Context -> AST -> AST -> IO AST
mkEq c e1 e2 = checkError c $ AST <$> z3_mk_eq (unContext c) (unAST e1) (unAST e2)

-- | The distinct construct is used for declaring the arguments pairwise
-- distinct.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaa076d3a668e0ec97d61744403153ecf7>
mkDistinct :: Context -> [AST] -> IO AST
mkDistinct _ [] = error "Z3.Base.mkDistinct: empty list of expressions"
mkDistinct c es =
  withArray (map unAST es) $ \aptr ->
    checkError c $
      AST <$> z3_mk_distinct (unContext c) n aptr
  where n = genericLength es

-- | Create an AST node representing /not(a)/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga3329538091996eb7b3dc677760a61072>
mkNot :: Context -> AST -> IO AST
mkNot c e = checkError c $ AST <$> z3_mk_not (unContext c) (unAST e)

-- | Create an AST node representing an if-then-else: /ite(t1, t2, t3)/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga94417eed5c36e1ad48bcfc8ad6e83547>
mkIte :: Context -> AST -> AST -> AST -> IO AST
mkIte c g e1 e2 =
  checkError c $ AST <$> z3_mk_ite (unContext c) (unAST g) (unAST e1) (unAST e2)

-- | Create an AST node representing /t1 iff t2/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga930a8e844d345fbebc498ac43a696042>
mkIff :: Context -> AST -> AST -> IO AST
mkIff c p q = checkError c $ AST <$> z3_mk_iff (unContext c) (unAST p) (unAST q)

-- | Create an AST node representing /t1 implies t2/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac829c0e25bbbd30343bf073f7b524517>
mkImplies :: Context -> AST -> AST -> IO AST
mkImplies c p q = checkError c $ AST <$> z3_mk_implies (unContext c) (unAST p) (unAST q)

-- | Create an AST node representing /t1 xor t2/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gacc6d1b848032dec0c4617b594d4229ec>
mkXor :: Context -> AST -> AST -> IO AST
mkXor c p q = checkError c $ AST <$> z3_mk_xor (unContext c) (unAST p) (unAST q)

-- | Create an AST node representing args[0] and ... and args[num_args-1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gacde98ce4a8ed1dde50b9669db4838c61>
mkAnd :: Context -> [AST] -> IO AST
mkAnd _ [] = error "Z3.Base.mkAnd: empty list of expressions"
mkAnd c es =
  withArray (map unAST es) $ \aptr ->
    checkError c $
      AST <$> z3_mk_and (unContext c) n aptr
  where n = genericLength es

-- | Create an AST node representing args[0] or ... or args[num_args-1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga00866d16331d505620a6c515302021f9>
mkOr :: Context -> [AST] -> IO AST
mkOr _ [] = error "Z3.Base.mkOr: empty list of expressions"
mkOr c es =
  withArray (map unAST es) $ \aptr ->
    checkError c $
      AST <$> z3_mk_or (unContext c) n aptr
  where n = genericLength es

-- | Create an AST node representing args[0] + ... + args[num_args-1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4e4ac0a4e53eee0b4b0ef159ed7d0cd5>
mkAdd :: Context -> [AST] -> IO AST
mkAdd _ [] = error "Z3.Base.mkAdd: empty list of expressions"
mkAdd c es =
  withArray (map unAST es) $ \aptr ->
    checkError c $
      AST <$> z3_mk_add (unContext c) n aptr
  where n = genericLength es

-- | Create an AST node representing args[0] * ... * args[num_args-1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab9affbf8401a18eea474b59ad4adc890>
mkMul :: Context -> [AST] -> IO AST
mkMul _ [] = error "Z3.Base.mkMul: empty list of expressions"
mkMul c es =
  withArray (map unAST es) $ \aptr ->
    checkError c $
      AST <$> z3_mk_mul (unContext c) n aptr
  where n = genericLength es

-- | Create an AST node representing args[0] - ... - args[num_args - 1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4f5fea9b683f9e674fd8f14d676cc9a9>
mkSub :: Context -> [AST] -> IO AST
mkSub _ [] = error "Z3.Base.mkSub: empty list of expressions"
mkSub c es =
  withArray (map unAST es) $ \aptr ->
    checkError c $
      AST <$> z3_mk_sub (unContext c) n aptr
  where n = genericLength es

-- | Create an AST node representing -arg.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gadcd2929ad732937e25f34277ce4988ea>
mkUnaryMinus :: Context -> AST -> IO AST
mkUnaryMinus c e = checkError c $ AST <$> z3_mk_unary_minus (unContext c) (unAST e)

-- | Create an AST node representing arg1 div arg2.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1ac60ee8307af8d0b900375914194ff3>
mkDiv :: Context -> AST -> AST -> IO AST
mkDiv c e1 e2 = checkError c $ AST <$> z3_mk_div (unContext c) (unAST e1) (unAST e2)

-- | Create an AST node representing arg1 mod arg2.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga8e350ac77e6b8fe805f57efe196e7713>
mkMod :: Context -> AST -> AST -> IO AST
mkMod c e1 e2 = checkError c $ AST <$> z3_mk_mod (unContext c) (unAST e1) (unAST e2)

-- | Create an AST node representing arg1 rem arg2.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga2fcdb17f9039bbdaddf8a30d037bd9ff>
mkRem :: Context -> AST -> AST -> IO AST
mkRem c e1 e2 = checkError c $ AST <$> z3_mk_rem (unContext c) (unAST e1) (unAST e2)

-- | Create less than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga58a3dc67c5de52cf599c346803ba1534>
mkLt :: Context -> AST -> AST -> IO AST
mkLt c e1 e2 = checkError c $ AST <$> z3_mk_lt (unContext c) (unAST e1) (unAST e2)

-- | Create less than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaa9a33d11096841f4e8c407f1578bc0bf>
mkLe :: Context -> AST -> AST -> IO AST
mkLe c e1 e2 = checkError c $ AST <$> z3_mk_le (unContext c) (unAST e1) (unAST e2)

-- | Create greater than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga46167b86067586bb742c0557d7babfd3>
mkGt :: Context -> AST -> AST -> IO AST
mkGt c e1 e2 = checkError c $ AST <$> z3_mk_gt (unContext c) (unAST e1) (unAST e2)

-- | Create greater than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gad9245cbadb80b192323d01a8360fb942>
mkGe :: Context -> AST -> AST -> IO AST
mkGe c e1 e2 = checkError c $ AST <$> z3_mk_ge (unContext c) (unAST e1) (unAST e2)

-- | Coerce an integer to a real.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga7130641e614c7ebafd28ae16a7681a21>
mkInt2Real :: Context -> AST -> IO AST
mkInt2Real c e = checkError c $ AST <$> z3_mk_int2real (unContext c) (unAST e)

-- | Coerce a real to an integer.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga759b6563ba1204aae55289009a3fdc6d>
mkReal2Int :: Context -> AST -> IO AST
mkReal2Int c e = checkError c $ AST <$> z3_mk_real2int (unContext c) (unAST e)

-- | Check if a real number is an integer.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaac2ad0fb04e4900fdb4add438d137ad3>
mkIsInt :: Context -> AST -> IO AST
mkIsInt c e = checkError c $ AST <$> z3_mk_is_int (unContext c) (unAST e)

---------------------------------------------------------------------
-- Bit-vectors

-- | Bitwise negation.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga36cf75c92c54c1ca633a230344f23080>
mkBvnot :: Context -> AST -> IO AST
mkBvnot c e = checkError c $ AST <$> z3_mk_bvnot (unContext c) (unAST e)

-- | Take conjunction of bits in vector, return vector of length 1.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaccc04f2b58903279b1b3be589b00a7d8>
mkBvredand :: Context -> AST -> IO AST
mkBvredand c e = checkError c $ AST <$> z3_mk_bvredand (unContext c) (unAST e)

-- | Take disjunction of bits in vector, return vector of length 1.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gafd18e127c0586abf47ad9cd96895f7d2>
mkBvredor :: Context -> AST -> IO AST
mkBvredor c e = checkError c $ AST <$> z3_mk_bvredor (unContext c) (unAST e)

-- | Bitwise and.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab96e0ea55334cbcd5a0e79323b57615d>
mkBvand :: Context -> AST -> AST -> IO AST
mkBvand c e1 e2 = checkError c $ AST <$> z3_mk_bvand (unContext c) (unAST e1) (unAST e2)

-- | Bitwise or.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga77a6ae233fb3371d187c6d559b2843f5>
mkBvor :: Context -> AST -> AST -> IO AST
mkBvor c e1 e2 = checkError c $ AST <$> z3_mk_bvor (unContext c) (unAST e1) (unAST e2)

-- | Bitwise exclusive-or.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0a3821ea00b1c762205f73e4bc29e7d8>
mkBvxor :: Context -> AST -> AST -> IO AST
mkBvxor c e1 e2 = checkError c $ AST <$> z3_mk_bvxor (unContext c) (unAST e1) (unAST e2)

-- | Bitwise nand.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga96dc37d36efd658fff5b2b4df49b0e61>
mkBvnand :: Context -> AST -> AST -> IO AST
mkBvnand c e1 e2 = checkError c $ AST <$> z3_mk_bvnand (unContext c) (unAST e1) (unAST e2)

-- | Bitwise nor.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gabf15059e9e8a2eafe4929fdfd259aadb>
mkBvnor :: Context -> AST -> AST -> IO AST
mkBvnor c e1 e2 = checkError c $ AST <$> z3_mk_bvnor (unContext c) (unAST e1) (unAST e2)

-- | Bitwise xnor.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga784f5ca36a4b03b93c67242cc94b21d6>
mkBvxnor :: Context -> AST -> AST -> IO AST
mkBvxnor c e1 e2 = checkError c $ AST <$> z3_mk_bvxnor (unContext c) (unAST e1) (unAST e2)

-- | Standard two's complement unary minus.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0c78be00c03eda4ed6a983224ed5c7b7
mkBvneg :: Context -> AST -> IO AST
mkBvneg c e = checkError c $ AST <$> z3_mk_bvneg (unContext c) (unAST e)

-- | Standard two's complement addition.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga819814e33573f3f9948b32fdc5311158>
mkBvadd :: Context -> AST -> AST -> IO AST
mkBvadd c e1 e2 = checkError c $ AST <$> z3_mk_bvadd (unContext c) (unAST e1) (unAST e2)

-- | Standard two's complement subtraction.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga688c9aa1347888c7a51be4e46c19178e>
mkBvsub :: Context -> AST -> AST -> IO AST
mkBvsub c e1 e2 = checkError c $ AST <$> z3_mk_bvsub (unContext c) (unAST e1) (unAST e2)

-- | Standard two's complement multiplication.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga6abd3dde2a1ceff1704cf7221a72258c>
mkBvmul :: Context -> AST -> AST -> IO AST
mkBvmul c e1 e2 = checkError c $ AST <$> z3_mk_bvmul (unContext c) (unAST e1) (unAST e2)

-- | Unsigned division.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga56ce0cd61666c6f8cf5777286f590544>
mkBvudiv :: Context -> AST -> AST -> IO AST
mkBvudiv c e1 e2 = checkError c $ AST <$> z3_mk_bvudiv (unContext c) (unAST e1) (unAST e2)

-- | Two's complement signed division.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gad240fedb2fda1c1005b8e9d3c7f3d5a0>
mkBvsdiv :: Context -> AST -> AST -> IO AST
mkBvsdiv c e1 e2 = checkError c $ AST <$> z3_mk_bvsdiv (unContext c) (unAST e1) (unAST e2)

-- | Unsigned remainder.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5df4298ec835e43ddc9e3e0bae690c8d>
mkBvurem :: Context -> AST -> AST -> IO AST
mkBvurem c e1 e2 = checkError c $ AST <$> z3_mk_bvurem (unContext c) (unAST e1) (unAST e2)

-- | Two's complement signed remainder (sign follows dividend).
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga46c18a3042fca174fe659d3185693db1>
mkBvsrem :: Context -> AST -> AST -> IO AST
mkBvsrem c e1 e2 = checkError c $ AST <$> z3_mk_bvsrem (unContext c) (unAST e1) (unAST e2)

-- | Two's complement signed remainder (sign follows divisor).
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga95dac8e6eecb50f63cb82038560e0879>
mkBvsmod :: Context -> AST -> AST -> IO AST
mkBvsmod c e1 e2 = checkError c $ AST <$> z3_mk_bvsmod (unContext c) (unAST e1) (unAST e2)

-- | Unsigned less than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5774b22e93abcaf9b594672af6c7c3c4>
mkBvult :: Context -> AST -> AST -> IO AST
mkBvult c e1 e2 = checkError c $ AST <$> z3_mk_bvult (unContext c) (unAST e1) (unAST e2)

-- | Two's complement signed less than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga8ce08af4ed1fbdf08d4d6e63d171663a>
mkBvslt :: Context -> AST -> AST -> IO AST
mkBvslt c e1 e2 = checkError c $ AST <$> z3_mk_bvslt (unContext c) (unAST e1) (unAST e2)

-- | Unsigned less than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab738b89de0410e70c089d3ac9e696e87>
mkBvule :: Context -> AST -> AST -> IO AST
mkBvule c e1 e2 = checkError c $ AST <$> z3_mk_bvule (unContext c) (unAST e1) (unAST e2)

-- | Two's complement signed less than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab7c026feb93e7d2eab180e96f1e6255d>
mkBvsle :: Context -> AST -> AST -> IO AST
mkBvsle c e1 e2 = checkError c $ AST <$> z3_mk_bvsle (unContext c) (unAST e1) (unAST e2)

-- | Unsigned greater than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gade58fbfcf61b67bf8c4a441490d3c4df>
mkBvuge :: Context -> AST -> AST -> IO AST
mkBvuge c e1 e2 = checkError c $ AST <$> z3_mk_bvuge (unContext c) (unAST e1) (unAST e2)

-- | Two's complement signed greater than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaeec3414c0e8a90a6aa5a23af36bf6dc5>
mkBvsge :: Context -> AST -> AST -> IO AST
mkBvsge c e1 e2 = checkError c $ AST <$> z3_mk_bvsge (unContext c) (unAST e1) (unAST e2)

-- | Unsigned greater than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga063ab9f16246c99e5c1c893613927ee3>
mkBvugt :: Context -> AST -> AST -> IO AST
mkBvugt c e1 e2 = checkError c $ AST <$> z3_mk_bvugt (unContext c) (unAST e1) (unAST e2)

-- | Two's complement signed greater than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4e93a985aa2a7812c7c11a2c65d7c5f0>
mkBvsgt :: Context -> AST -> AST -> IO AST
mkBvsgt c e1 e2 = checkError c $ AST <$> z3_mk_bvsgt (unContext c) (unAST e1) (unAST e2)

-- | Concatenate the given bit-vectors.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae774128fa5e9ff7458a36bd10e6ca0fa>
mkConcat :: Context -> AST -> AST -> IO AST
mkConcat c e1 e2 = checkError c $ AST <$> z3_mk_concat (unContext c) (unAST e1) (unAST e2)

-- | Extract the bits high down to low from a bitvector of size m to yield a new
-- bitvector of size /n/, where /n = high - low + 1/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga32d2fe7563f3e6b114c1b97b205d4317>
mkExtract :: Context -> Int -> Int -> AST -> IO AST
mkExtract c j i e
  = checkError c $ AST <$> z3_mk_extract (unContext c) (fromIntegral j) (fromIntegral i) (unAST e)

-- | Sign-extend of the given bit-vector to the (signed) equivalent bitvector
-- of size /m+i/, where /m/ is the size of the given bit-vector.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gad29099270b36d0680bb54b560353c10e>
mkSignExt :: Context -> Int -> AST -> IO AST
mkSignExt c i e
  = checkError c $ AST <$> z3_mk_sign_ext (unContext c) (fromIntegral i) (unAST e)

-- | Extend the given bit-vector with zeros to the (unsigned) equivalent
-- bitvector of size /m+i/, where /m/ is the size of the given bit-vector.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac9322fae11365a78640baf9078c428b3>
mkZeroExt :: Context -> Int -> AST -> IO AST
mkZeroExt c i e
  = checkError c $ AST <$> z3_mk_zero_ext (unContext c) (fromIntegral i) (unAST e)

-- | Repeat the given bit-vector up length /i/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga03e81721502ea225c264d1f556c9119d>
mkRepeat :: Context -> Int -> AST -> IO AST
mkRepeat c i e
  = checkError c $ AST <$> z3_mk_repeat (unContext c) (fromIntegral i) (unAST e)

-- | Shift left.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac8d5e776c786c1172fa0d7dfede454e1>
mkBvshl :: Context -> AST -> AST -> IO AST
mkBvshl c e1 e2 = checkError c $ AST <$> z3_mk_bvshl (unContext c) (unAST e1) (unAST e2)

-- | Logical shift right.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac59645a6edadad79a201f417e4e0c512>
mkBvlshr :: Context -> AST -> AST -> IO AST
mkBvlshr c e1 e2 = checkError c $ AST <$> z3_mk_bvlshr (unContext c) (unAST e1) (unAST e2)

-- | Arithmetic shift right.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga674b580ad605ba1c2c9f9d3748be87c4>
mkBvashr :: Context -> AST -> AST -> IO AST
mkBvashr c e1 e2 = checkError c $ AST <$> z3_mk_bvashr (unContext c) (unAST e1) (unAST e2)

-- | Rotate bits of /t1/ to the left /i/ times.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4932b7d08fea079dd903cd857a52dcda>
mkRotateLeft :: Context -> Int -> AST -> IO AST
mkRotateLeft c i e
  = checkError c $ AST <$> z3_mk_rotate_left (unContext c) (fromIntegral i) (unAST e)

-- | Rotate bits of /t1/ to the right /i/ times.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga3b94e1bf87ecd1a1858af8ebc1da4a1c>
mkRotateRight :: Context -> Int -> AST -> IO AST
mkRotateRight c i e
  = checkError c $ AST <$> z3_mk_rotate_right (unContext c) (fromIntegral i) (unAST e)

-- | Rotate bits of /t1/ to the left /t2/ times.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaf46f1cb80e5a56044591a76e7c89e5e7>
mkExtRotateLeft :: Context -> AST -> AST -> IO AST
mkExtRotateLeft c e1 e2
  = checkError c $ AST <$> z3_mk_ext_rotate_left (unContext c) (unAST e1) (unAST e2)

-- | Rotate bits of /t1/ to the right /t2/ times.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gabb227526c592b523879083f12aab281f>
mkExtRotateRight :: Context -> AST -> AST -> IO AST
mkExtRotateRight c e1 e2
  = checkError c $ AST <$> z3_mk_ext_rotate_right (unContext c) (unAST e1) (unAST e2)

-- | Create an /n/ bit bit-vector from the integer argument /t1/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga35f89eb05df43fbd9cce7200cc1f30b5>
mkInt2bv :: Context -> Int -> AST -> IO AST
mkInt2bv c i e
  = checkError c $ AST <$> z3_mk_int2bv (unContext c) (fromIntegral i) (unAST e)

-- | Create an integer from the bit-vector argument /t1/. If /is_signed/ is false,
-- then the bit-vector /t1/ is treated as unsigned. So the result is non-negative
-- and in the range [0..2^/N/-1], where /N/ are the number of bits in /t1/.
-- If /is_signed/ is true, /t1/ is treated as a signed bit-vector.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac87b227dc3821d57258d7f53a28323d4>
mkBv2int :: Context -> AST -> Bool -> IO AST
mkBv2int c e is_signed
  = checkError c $ AST <$> z3_mk_bv2int (unContext c) (unAST e) (unBool is_signed)

-- | Create a predicate that checks that the bit-wise addition of /t1/ and /t2/
-- does not overflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga88f6b5ec876f05e0d7ba51e96c4b077f>
mkBvaddNoOverflow :: Context -> AST -> AST -> Bool -> IO AST
mkBvaddNoOverflow c e1 e2 is_signed =
  checkError c $ AST <$> z3_mk_bvadd_no_overflow (unContext c) (unAST e1) (unAST e2)
                                                 (unBool is_signed)

-- | Create a predicate that checks that the bit-wise signed addition of /t1/
-- and /t2/ does not underflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1e2b1927cf4e50000c1600d47a152947>
mkBvaddNoUnderflow :: Context -> AST -> AST -> IO AST
mkBvaddNoUnderflow c e1 e2 =
  checkError c $ AST <$> z3_mk_bvadd_no_underflow (unContext c) (unAST e1) (unAST e2)

-- | Create a predicate that checks that the bit-wise signed subtraction of /t1/
-- and /t2/ does not overflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga785f8127b87e0b42130e6d8f52167d7c>
mkBvsubNoOverflow :: Context -> AST -> AST -> IO AST
mkBvsubNoOverflow c e1 e2 =
  checkError c $ AST <$> z3_mk_bvsub_no_overflow (unContext c) (unAST e1) (unAST e2)

-- | Create a predicate that checks that the bit-wise subtraction of /t1/ and
-- /t2/ does not underflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga6480850f9fa01e14aea936c88ff184c4>
mkBvsubNoUnderflow :: Context -> AST -> AST -> IO AST
mkBvsubNoUnderflow c e1 e2 =
  checkError c $ AST <$> z3_mk_bvsub_no_underflow (unContext c) (unAST e1) (unAST e2)

-- | Create a predicate that checks that the bit-wise signed division of /t1/
-- and /t2/ does not overflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaa17e7b2c33dfe2abbd74d390927ae83e>
mkBvsdivNoOverflow :: Context -> AST -> AST -> IO AST
mkBvsdivNoOverflow c e1 e2 =
  checkError c $ AST <$> z3_mk_bvsdiv_no_overflow (unContext c) (unAST e1) (unAST e2)

-- | Check that bit-wise negation does not overflow when /t1/ is interpreted as
-- a signed bit-vector.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae9c5d72605ddcd0e76657341eaccb6c7>
mkBvnegNoOverflow :: Context -> AST -> IO AST
mkBvnegNoOverflow c e =
  checkError c $ AST <$> z3_mk_bvneg_no_overflow (unContext c) (unAST e)

-- | Create a predicate that checks that the bit-wise multiplication of /t1/ and
-- /t2/ does not overflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga86f4415719d295a2f6845c70b3aaa1df>
mkBvmulNoOverflow :: Context -> AST -> AST -> Bool -> IO AST
mkBvmulNoOverflow c e1 e2 is_signed =
  checkError c $ AST <$> z3_mk_bvmul_no_overflow (unContext c) (unAST e1) (unAST e2)
                                                 (unBool is_signed)

-- | Create a predicate that checks that the bit-wise signed multiplication of
-- /t1/ and /t2/ does not underflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga501ccc01d737aad3ede5699741717fda>
mkBvmulNoUnderflow :: Context -> AST -> AST -> IO AST
mkBvmulNoUnderflow c e1 e2 =
  checkError c $ AST <$> z3_mk_bvmul_no_underflow (unContext c) (unAST e1) (unAST e2)

---------------------------------------------------------------------
-- Arrays

-- | Array read. The argument a is the array and i is the index of the array
-- that gets read.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga38f423f3683379e7f597a7fe59eccb67>
mkSelect :: Context -> AST -> AST -> IO AST
mkSelect c a1 a2 = checkError c $ AST <$> z3_mk_select (unContext c) (unAST a1) (unAST a2)

-- | Array update.   
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae305a4f54b4a64f7e5973ae6ccb13593>
mkStore :: Context -> AST -> AST -> AST -> IO AST
mkStore c a1 a2 a3 = checkError c $ AST <$> z3_mk_store (unContext c) (unAST a1) (unAST a2) (unAST a3)

-- | Create the constant array.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga84ea6f0c32b99c70033feaa8f00e8f2d>
mkConstArray :: Context -> Sort -> AST -> IO AST
mkConstArray c s a = checkError c $ AST <$> z3_mk_const_array (unContext c) (unSort s) (unAST a)

-- | map f on the the argument arrays.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga9150242d9430a8c3d55d2ca3b9a4362d>
mkMap :: Context -> FuncDecl -> Int -> [AST] -> IO AST
mkMap c f n args = withArray (map unAST args) $ \args' ->
    checkError c $ AST <$> z3_mk_map (unContext c) (unFuncDecl f) (fromIntegral n) args'

-- | Access the array default value. Produces the default range value, for
-- arrays that can be represented as finite maps with a default range value.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga78e89cca82f0ab4d5f4e662e5e5fba7d>
mkArrayDefault :: Context -> AST -> IO AST
mkArrayDefault c a = checkError c $ AST <$> z3_mk_array_default (unContext c) (unAST a)

-- TODO Sets

---------------------------------------------------------------------
-- Numerals

-- | Create a numeral of a given sort.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac8aca397e32ca33618d8024bff32948c>
mkNumeral :: Context -> String -> Sort -> IO AST
mkNumeral c str s =
  withCString str $ \cstr->
    checkError c $ AST <$> z3_mk_numeral (unContext c) cstr (unSort s)

-------------------------------------------------
-- Numerals / Integers

-- | Create a numeral of sort /int/.
mkInt :: Integral a => Context -> a -> IO AST
mkInt c n = mkIntSort c >>= mkNumeral c n_str
  where n_str = show $ toInteger n

{-# INLINE mkIntZ3 #-}
mkIntZ3 :: Context -> Int32 -> Sort -> IO AST
mkIntZ3 c n s = checkError c $ AST <$> z3_mk_int (unContext c) cn (unSort s)
  where cn = fromIntegral n :: CInt

{-# INLINE mkUnsignedIntZ3 #-}
mkUnsignedIntZ3 :: Context -> Word32 -> Sort -> IO AST
mkUnsignedIntZ3 c n s = checkError c $ AST <$> z3_mk_unsigned_int (unContext c) cn (unSort s)
  where cn = fromIntegral n :: CUInt

{-# INLINE mkInt64Z3 #-}
mkInt64Z3 :: Context -> Int64 -> Sort -> IO AST
mkInt64Z3 c n s = checkError c $ AST <$> z3_mk_int64 (unContext c) cn (unSort s)
  where cn = fromIntegral n :: CLLong

{-# INLINE mkUnsignedInt64Z3 #-}
mkUnsignedInt64Z3 :: Context -> Word64 -> Sort -> IO AST
mkUnsignedInt64Z3 c n s =
  checkError c $ AST <$> z3_mk_unsigned_int64 (unContext c) cn (unSort s)
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
mkRealZ3 c r = checkError c $ AST <$> z3_mk_real (unContext c) n d
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
    checkError c $ Pattern <$> z3_mk_pattern (unContext c) n aptr
  where n = genericLength es

mkBound :: Context -> Int -> Sort -> IO AST
mkBound c i s
  | i >= 0    = checkError c $ AST <$> z3_mk_bound (unContext c) (fromIntegral i) (unSort s)
  | otherwise = error "Z3.Base.mkBound: negative de-Bruijn index"

mkForall :: Context -> [Pattern] -> [Symbol] -> [Sort] -> AST -> IO AST
mkForall c pats x s p
  = withArray (map unPattern pats) $ \patsPtr ->
    withArray (map unSymbol  x   ) $ \xptr ->
    withArray (map unSort    s   ) $ \sptr ->
      checkError c $ AST <$> z3_mk_forall cptr 0 n patsPtr len sptr xptr (unAST p)
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
      checkError c $ AST <$> z3_mk_exists cptr 0 n patsPtr len sptr xptr (unAST p)
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
  checkError c $ fromIntegral <$> z3_get_bv_sort_size (unContext c) (unSort s)

-- | Return the sort of an AST node.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0a4dac7e9397ff067136354cd33cb933>
getSort :: Context -> AST -> IO Sort
getSort c e = checkError c $ Sort <$> z3_get_sort (unContext c) (unAST e)

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
getBool c a = checkError c $
  castLBool <$> z3_get_bool_value (unContext c) (unAST a)

-- | Return numeral value, as a string of a numeric constant term.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga94617ef18fa7157e1a3f85db625d2f4b>
getNumeralString :: Context -> AST -> IO String
getNumeralString c a = checkError c $ peekCString =<< z3_get_numeral_string ctxPtr (unAST a)
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
    checkError ctx $ z3_eval ctxPtr (unModel m) (unAST a) aptr2 >>= peekAST aptr2 . toBool
  where peekAST :: Ptr (Ptr Z3_ast) -> Bool -> IO (Maybe AST)
        peekAST _p False = return Nothing
        peekAST  p True  = Just . AST <$> peek p

        ctxPtr = unContext ctx

-- | Evaluate a collection of AST nodes in the given model.
evalT :: Traversable t => Context -> Model -> t AST -> IO (Maybe (t AST))
evalT c m = fmap T.sequence . T.mapM (eval c m)

-- | The interpretation of a function is a mapping part (arguments to values)
-- and an 'else' part.
data FuncModel = FuncModel
    { interpMap :: [([AST], AST)]
    , interpElse :: AST
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
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga7d9262dc6e79f2aeb23fd4a383589dda>
getAsArrayFuncDecl :: Context -> AST -> IO FuncDecl
getAsArrayFuncDecl ctx a = checkError ctx $
  FuncDecl <$> z3_get_as_array_func_decl (unContext ctx) (unAST a)

-- | The (_ as-array f) AST node is a construct for assigning interpretations
-- for arrays in Z3. It is the array such that forall indices i we have that
-- (select (_ as-array f) i) is equal to (f i). This procedure returns Z3_TRUE
-- if the a is an as-array AST node.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4674da67d226bfb16861829b9f129cfa>
isAsArray :: Context -> AST -> IO Bool
isAsArray ctx a = toBool <$> z3_is_as_array (unContext ctx) (unAST a)


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
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gafb9cc5eca9564d8a849c154c5a4a8633>
getFuncInterp :: Context -> Model -> FuncDecl -> IO (Maybe FuncInterp)
getFuncInterp c m fd = do
  ptr <- checkError c $
           z3_model_get_func_interp (unContext c) (unModel m) (unFuncDecl fd)
  return $ FuncInterp <$> ptrToMaybe ptr

-- | Return the number of entries in the given function interpretation.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga2bab9ae1444940e7593729beec279844>
funcInterpGetNumEntries :: Context -> FuncInterp -> IO Int
funcInterpGetNumEntries ctx fi = checkError ctx $ fromIntegral <$>
  z3_func_interp_get_num_entries (unContext ctx) (unFuncInterp fi)

-- | Return a _point_ of the given function intepretation.
-- It represents the value of f in a particular point.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaf157e1e1cd8c0cfe6a21be6370f659da>
funcInterpGetEntry :: Context -> FuncInterp -> Int -> IO FuncEntry
funcInterpGetEntry ctx fi i = checkError ctx $ FuncEntry <$>
  z3_func_interp_get_entry (unContext ctx) (unFuncInterp fi) (fromIntegral i)

-- | Return the 'else' value of the given function interpretation.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga46de7559826ba71b8488d727cba1fb64>
funcInterpGetElse :: Context -> FuncInterp -> IO AST
funcInterpGetElse ctx fi = checkError ctx $
  AST <$> z3_func_interp_get_else (unContext ctx) (unFuncInterp fi)

-- | Return the arity (number of arguments) of the given function
-- interpretation.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaca22cbdb6f7787aaae5d814f2ab383d8>
funcInterpGetArity :: Context -> FuncInterp -> IO Int
funcInterpGetArity ctx fi = checkError ctx $
  fromIntegral <$> z3_func_interp_get_arity (unContext ctx) (unFuncInterp fi)

-- | Return the value of this point.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga9fd65e2ab039aa8e40608c2ecf7084da>
funcEntryGetValue :: Context -> FuncEntry -> IO AST
funcEntryGetValue ctx entry = checkError ctx $
  AST <$> z3_func_entry_get_value (unContext ctx) (unFuncEntry entry)

-- | Return the number of arguments in a Z3_func_entry object.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga51aed8c5bc4b1f53f0c371312de3ce1a>
funcEntryGetNumArgs :: Context -> FuncEntry -> IO Int
funcEntryGetNumArgs ctx entry = checkError ctx $ fromIntegral <$>
  z3_func_entry_get_num_args (unContext ctx) (unFuncEntry entry)

-- | Return an argument of a Z3_func_entry object.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga6fe03fe3c824fceb52766a4d8c2cbeab>
funcEntryGetArg :: Context -> FuncEntry -> Int -> IO AST
funcEntryGetArg ctx entry i = checkError ctx $ AST <$>
  z3_func_entry_get_arg (unContext ctx) (unFuncEntry entry) (fromIntegral i)

-- | Convert the given model into a string.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaf36d49862a8c0d20dd5e6508eef5f8af>
modelToString :: Context -> Model -> IO String
modelToString c m = checkError c $
  z3_model_to_string (unContext c) (unModel m) >>= peekCString

-- | Alias for 'modelToString'.
showModel :: Context -> Model -> IO String
showModel = modelToString

---------------------------------------------------------------------
-- Constraints

-- | Create a backtracking point.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gad651ad68c7a060cbb5616349233cb10f>
push :: Context -> IO ()
push c = checkError c $ z3_push $ unContext c

-- | Backtrack /n/ backtracking points.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab2b3a542006c86c8d86dc37872f88b61>
pop :: Context -> Int -> IO ()
pop ctx cnt = checkError ctx $ z3_pop (unContext ctx) $ fromIntegral cnt

-- TODO Constraints: Z3_get_num_scopes
-- TODO Constraints: Z3_persist_ast

-- | Assert a constraing into the logical context.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1a05ff73a564ae7256a2257048a4680a>
assertCnstr :: Context -> AST -> IO ()
assertCnstr ctx ast = checkError ctx $
  z3_assert_cnstr (unContext ctx) (unAST ast)

-- | Get model (logical context is consistent)
--
-- Reference : <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaff310fef80ac8a82d0a51417e073ec0a>
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

-- | Delete a model object.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0cc98d3ce68047f873e119bccaabdbee>
delModel :: Context -> Model -> IO ()
delModel c m = checkError c $ z3_del_model (unContext c) (unModel m)

-- | Check whether the given logical context is consistent or not.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga72055cfbae81bd174abed32a83e50b03>
check :: Context -> IO Result
check ctx = checkError ctx $ toResult <$> z3_check (unContext ctx)

-- TODO Constraints: Z3_check_assumptions
-- TODO Constraints: Z3_get_implied_equalities

-- TODO From section 'Constraints' on.


---------------------------------------------------------------------
-- * Parameters

mkParams :: Context -> IO Params
mkParams c = checkError c $ Params <$> z3_mk_params (unContext c)

paramsSetBool :: Context -> Params -> Symbol -> Bool -> IO ()
paramsSetBool c params sym b = checkError c $
  z3_params_set_bool (unContext c) (unParams params) (unSymbol sym) (unBool b)

paramsSetUInt :: Context -> Params -> Symbol -> Int -> IO ()
paramsSetUInt c params sym v = checkError c $
  z3_params_set_uint (unContext c) (unParams params)
        (unSymbol sym) (fromIntegral v)

paramsSetDouble :: Context -> Params -> Symbol -> Double -> IO ()
paramsSetDouble c params sym v = checkError c $
  z3_params_set_double (unContext c) (unParams params)
        (unSymbol sym) (realToFrac v)

paramsSetSymbol :: Context -> Params -> Symbol -> Symbol -> IO ()
paramsSetSymbol c params sym v =
  checkError c $ z3_params_set_symbol (unContext c) (unParams params) (unSymbol sym)
                                      (unSymbol v)

paramsToString :: Context -> Params -> IO String
paramsToString c (Params params) =
  checkError c (z3_params_to_string (unContext c) params) >>= peekCString


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
mkSolver c = checkError c $ Solver <$> z3_mk_solver (unContext c)

mkSimpleSolver :: Context -> IO Solver
mkSimpleSolver c = checkError c $ Solver <$> z3_mk_simple_solver (unContext c)

mkSolverForLogic :: Context -> Logic -> IO Solver
mkSolverForLogic c logic =
  do sym <- mkStringSymbol c (show logic)
     checkError c $ Solver <$> z3_mk_solver_for_logic (unContext c) (unSymbol sym)

solverSetParams :: Context -> Solver -> Params -> IO ()
solverSetParams c solver params =
  checkError c $ z3_solver_set_params (unContext c) (unSolver solver) (unParams params)

solverPush :: Context -> Solver -> IO ()
solverPush c solver = checkError c $ z3_solver_push (unContext c) (unSolver solver)

solverPop :: Context -> Solver -> Int -> IO ()
solverPop c solver i =
  checkError c $ z3_solver_pop (unContext c) (unSolver solver) (fromIntegral i)

solverReset :: Context -> Solver -> IO ()
solverReset c solver = checkError c $ z3_solver_reset (unContext c) (unSolver solver)

solverAssertCnstr :: Context -> Solver -> AST -> IO ()
solverAssertCnstr c solver ast =
  checkError c $ z3_solver_assert (unContext c) (unSolver solver) (unAST ast)

solverAssertAndTrack :: Context -> Solver -> AST -> AST -> IO ()
solverAssertAndTrack c solver constraint var =
  checkError c $ z3_solver_assert_and_track (unContext c) (unSolver solver)
                                            (unAST constraint) (unAST var)

solverCheck :: Context -> Solver -> IO Result
solverCheck c solver =
  checkError c $ toResult <$> z3_solver_check (unContext c) (unSolver solver)

solverCheckAndGetModel :: Context -> Solver -> IO (Result, Maybe Model)
solverCheckAndGetModel c (Solver s) =
  do res <- checkError c $ toResult <$> z3_solver_check cptr s
     mmodel <- case res of
                 Sat -> checkError c $ (Just . Model) <$> z3_solver_get_model cptr s
                 _ -> return Nothing
     return (res, mmodel)
  where cptr = unContext c

solverGetReasonUnknown :: Context -> Solver -> IO String
solverGetReasonUnknown c solver =
  checkError c $ z3_solver_get_reason_unknown (unContext c) (unSolver solver) >>= peekCString

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
astToString ctx ast =
  checkError ctx $ z3_ast_to_string (unContext ctx) (unAST ast) >>= peekCString

-- | Convert a pattern to a string.
patternToString :: Context -> Pattern -> IO String
patternToString ctx pattern =
  checkError ctx $ z3_pattern_to_string (unContext ctx) (unPattern pattern) >>= peekCString

-- | Convert a sort to a string.
sortToString :: Context -> Sort -> IO String
sortToString ctx sort =
  checkError ctx $ z3_sort_to_string (unContext ctx) (unSort sort) >>= peekCString

-- | Convert a FuncDecl to a string.
funcDeclToString :: Context -> FuncDecl -> IO String
funcDeclToString ctx funcDecl =
  checkError ctx $ z3_func_decl_to_string (unContext ctx) (unFuncDecl funcDecl) >>= peekCString

-- | Convert the given benchmark into SMT-LIB formatted string.
--
-- The output format can be configured via 'setASTPrintMode'.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaf93844a5964ad8dee609fac3470d86e4>
benchmarkToSMTLibString :: Context
                            -> String   -- ^ name
                            -> String   -- ^ logic
                            -> String   -- ^ status
                            -> String   -- ^ attributes
                            -> [AST]    -- ^ assumptions
                            -> AST      -- ^ formula
                            -> IO String
benchmarkToSMTLibString c name logic st attr assump f =
  withCString name $ \cname ->
  withCString logic $ \clogic ->
  withCString st $ \cst ->
  withCString attr $ \cattr ->
  withArray (map unAST assump) $ \cassump ->
    z3_benchmark_to_smtlib_string (unContext c) cname clogic cst cattr
                                  numAssump cassump (unAST f)
      >>= peekCString
  where numAssump = genericLength assump

---------------------------------------------------------------------
-- Utils

-- | Wraps a non-null pointer with 'Just', or else returns 'Nothing'.
ptrToMaybe :: Ptr a -> Maybe (Ptr a)
ptrToMaybe ptr | ptr == nullPtr = Nothing
               | otherwise      = Just ptr


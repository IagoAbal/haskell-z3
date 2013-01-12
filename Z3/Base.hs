{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances        #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE UndecidableInstances       #-}

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

    -- ** Satisfiability result
    , Result

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

-- TODO Sorts: from Z3_mk_bv_sort on

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
mkSub ::Context -> [AST] -> IO AST
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

-- TODO Bit-vector, Arrays, Sets

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
      AST <$> z3_mk_forall cptr 0 n patsPtr 1 sptr xptr (unAST p)
  where n    = genericLength pats
        cptr = unContext c

mkExists :: Context -> [Pattern] -> [Symbol] -> [Sort] -> AST -> IO AST
mkExists c pats x s p
  = withArray (map unPattern pats) $ \patsPtr ->
    withArray (map unSymbol  x   ) $ \xptr ->
    withArray (map unSort    s   ) $ \sptr ->
      AST <$> z3_mk_exists cptr 0 n patsPtr 1 sptr xptr (unAST p)
  where n    = fromIntegral $ length pats
        cptr = unContext c

---------------------------------------------------------------------
-- Accessors

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

-- TODO Constraints: Z3_push
push :: Context -> IO ()
push = z3_push . unContext

pop :: Context -> Int -> IO ()
pop ctx cnt = z3_pop (unContext ctx) $ fromIntegral cnt

-- TODO Constraints: Z3_pop
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
                    | otherwise    = Just . Model <$> peek p

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

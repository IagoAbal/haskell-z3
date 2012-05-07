{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Module    : Z3.Base
-- Copyright : (c) Iago Abal, 2011 
--             (c) David Castro, 2011
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>, 
--             David Castro <david.castro.dcp@gmail.com>

module Z3.Base (

    -- * Core Z3 types
      Config
    , Context
    , Symbol
    , AST
    , Sort
    , App
    , Pattern
    , Model
    
    -- ** Utility types
    , Result(..)

    , castAST

    -- * Config
    , mkConfig
    , setParamValue

    -- * Context
    , mkContext

    -- * Symbols
    , mkStringSymbol

    -- * Sorts
    , mkBoolSort
    , mkIntSort
    , mkRealSort

    -- * Constants and Applications
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

    -- * Constraints
    , assertCnstr
    , check

    ) where

import Z3.Base.C
import Z3.Types hiding ( Sort )

import Control.Applicative ( (<$>) )
import Data.Int
import Data.Ratio ( Ratio, numerator, denominator )
import Data.Word
import Foreign hiding ( newForeignPtr )
import Foreign.C
import Foreign.Concurrent ( newForeignPtr )

------------------------------------------------------------------------
-- Types

-- | A Z3 /configuration object/.
-- 
--
-- /Notes:/
--
-- * The resource is automatically managed by the Haskell garbage
-- collector, and the structure is automatically deleted once it is out
-- of scope (no need to call 'z3_del_config').
--
-- /Reference:/ < TODO >
--
newtype Config = Config { unConfig :: ForeignPtr Z3_config }
    deriving Eq

-- | A Z3 /logical context/.
-- 
--
-- /Notes:/
--
-- * The resource is automatically managed by the Haskell garbage
-- collector, and the structure is automatically deleted once it is out
-- of scope (no need to call 'z3_del_context').
--
-- /Reference:/ < TODO >
--
newtype Context = Context { unContext :: ForeignPtr Z3_context }
    deriving Eq

-- | A Z3 /Lisp-link symbol/.
-- 
-- /Reference:/ < TODO >
--
newtype Symbol = Symbol { unSymbol :: Ptr Z3_symbol }
    deriving (Eq, Ord, Show, Storable)

-- | A Z3 /AST node/.
-- 
-- /Reference:/ < TODO >
--
newtype AST a = AST { unAST :: Ptr Z3_ast }
    deriving (Eq, Ord, Show, Storable)

-- | Cast an 'AST a' to 'AST b'
--
-- /Warning:/
-- * This cast is used to ignore the AST phantom type. It should
-- be used carefully.
--
castAST :: (Z3Type a, Z3Type b) => AST a -> AST b
castAST (AST a) = AST a

    -- TODO Improve type-safety with phantom types.

-- | Kind of Z3 AST representing /types/.
-- 
-- /Reference:/ < TODO >
--
newtype Sort a = Sort { unSort :: Ptr Z3_sort }
    deriving (Eq, Ord, Show, Storable)

-- | A kind of Z3 AST used to represent constant and function declarations.
-- 
-- /Reference:/ < TODO >
--
newtype App = App { _unApp :: Ptr Z3_app }
    deriving (Eq, Ord, Show, Storable)

-- | A kind of AST used to represent pattern and multi-patterns used to 
--   guide quantifier instantiation.
-- 
-- /Reference:/ < TODO >
--
newtype Pattern = Pattern { _unPattern :: Ptr Z3_pattern }
    deriving (Eq, Ord, Show, Storable)

-- | A model for the constraints asserted into the logical context.
-- 
-- /Reference:/ < TODO >
--
newtype Model = Model { _unModel :: Ptr Z3_model }
    deriving (Eq, Ord, Show, Storable)

-- | Lifted Boolean type.
--
-- /Reference:/ < TODO >
--
data Result
    = Satisfiable
    | Unsatisfiable
    | Undefined
    deriving (Eq, Ord, Enum, Bounded, Read, Show)

toResult :: Z3_lbool -> Result
toResult lb
    | lb == z3_l_true  = Satisfiable
    | lb == z3_l_false = Unsatisfiable
    | otherwise        = Undefined

---------------------------------------------------------------------
-- * Create configuration

-- | Create a configuration.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga7d6c40d9b79fe8a8851cc8540970787f>
--
mkConfig :: IO Config
mkConfig =
    z3_mk_config                          >>= \ptr  ->
    newForeignPtr ptr (z3_del_config ptr) >>= \fptr ->
        return $! Config fptr

-- | Set a configuration parameter.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga001ade87a1671fe77d7e53ed0f4f1ec3>
--
setParamValue :: Config -> String -> String -> IO ()
setParamValue cfg s1 s2 =
    withForeignPtr (unConfig cfg) $ \cfgPtr ->
    withCString s1                $ \cs1 ->
    withCString s2                $ \cs2 ->
        z3_set_param_value cfgPtr cs1 cs2

---------------------------------------------------------------------
-- * Create context

-- | Create a context using the given configuration.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0bd93cfab4d749dd3e2f2a4416820a46>
--
mkContext :: Config -> IO Context
mkContext cfg =
    withForeignPtr (unConfig cfg)              $ \cfgPtr ->
    z3_mk_context cfgPtr                     >>= \pCtx   ->
    newForeignPtr pCtx (z3_del_context pCtx) >>= \fptr   ->
        return $! Context fptr

---------------------------------------------------------------------
-- * Symbols

-- | Create a Z3 symbol using a C string.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gafebb0d3c212927cf7834c3a20a84ecae>
--
mkStringSymbol :: Context -> String -> IO Symbol
mkStringSymbol ctx s =
    withForeignPtr (unContext ctx) $ \ctxPtr ->
    withCString s                  $ \cs     ->
        Symbol <$> z3_mk_string_symbol ctxPtr cs

---------------------------------------------------------------------
-- * Sorts

-- TODO Sorts: Z3_is_eq_sort
-- TODO Sorts: Z3_mk_uninterpreted_sort

-- | Create the Boolean type.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gacdc73510b69a010b71793d429015f342>
--
mkBoolSort :: Context -> IO (Sort Bool)
mkBoolSort c = withForeignPtr (unContext c) $ \cptr ->
  Sort <$> z3_mk_bool_sort cptr

-- | Create an integer type.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga6cd426ab5748653b77d389fd3eac1015>
--
mkIntSort :: Z3Int i => Context -> IO (Sort i)
mkIntSort c = withForeignPtr (unContext c) $ \cptr ->
  Sort <$> z3_mk_int_sort cptr

-- | Create an real type.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga40ef93b9738485caed6dc84631c3c1a0>
--
mkRealSort :: Z3Real r => Context -> IO (Sort r)
mkRealSort c = withForeignPtr (unContext c) $ \cptr ->
  Sort <$> z3_mk_real_sort cptr

-- TODO Sorts: from Z3_mk_real_sort on


---------------------------------------------------------------------
-- * Constants and Applications

-- TODO Constants and Applications: Z3_is_eq_ast
-- TODO Constants and Applications: Z3_is_eq_func_decl
-- TODO Constants and Applications: Z3_mk_func_decl
-- TODO Constants and Applications: Z3_mk_app

-- | Declare and create a constant.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga093c9703393f33ae282ec5e8729354ef>
--
mkConst :: Context -> Symbol -> Sort a -> IO (AST a)
mkConst c x s = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_const cptr (unSymbol x) (unSort s)

-- TODO Constants and Applications: Z3_mk_label
-- TODO Constants and Applications: Z3_mk_fresh_func_decl
-- TODO Constants and Applications: Z3_mk_fresh_const

-- | Create an AST node representing /true/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae898e7380409bbc57b56cc5205ef1db7>
--
mkTrue :: Context -> IO (AST Bool)
mkTrue c = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_true cptr

-- | Create an AST node representing /false/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5952ac17671117a02001fed6575c778d>
--
mkFalse :: Context -> IO (AST Bool)
mkFalse c = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_false cptr

-- | Create an AST node representing l = r.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga95a19ce675b70e22bb0401f7137af37c>
--
mkEq :: Context -> AST a -> AST a -> IO (AST Bool)
mkEq c e1 e2 = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_eq cptr (unAST e1) (unAST e2)

-- | Create an AST node representing not(a).
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga3329538091996eb7b3dc677760a61072>
--
mkNot :: Context -> AST Bool -> IO (AST Bool)
mkNot c e = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_not cptr (unAST e)

-- | Create an AST node representing an if-then-else: ite(t1, t2, t3).
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga94417eed5c36e1ad48bcfc8ad6e83547>
--
mkIte :: Context -> AST Bool -> AST a -> AST a -> IO (AST a)
mkIte c g e1 e2 = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_ite cptr (unAST g) (unAST e1) (unAST e2)

-- | Create an AST node representing t1 iff t2. 
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga930a8e844d345fbebc498ac43a696042>
--
mkIff :: Context -> AST Bool -> AST Bool -> IO (AST Bool)
mkIff c p q = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_iff cptr (unAST p) (unAST q)

-- | Create an AST node representing t1 implies t2. 
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac829c0e25bbbd30343bf073f7b524517>
--
mkImplies :: Context -> AST Bool -> AST Bool -> IO (AST Bool)
mkImplies c p q = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_implies cptr (unAST p) (unAST q)

-- | Create an AST node representing t1 xor t2. 
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gacc6d1b848032dec0c4617b594d4229ec>
--
mkXor :: Context -> AST Bool -> AST Bool -> IO (AST Bool)
mkXor c p q = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_xor cptr (unAST p) (unAST q)

-- | Create an AST node representing args[0] and ... and args[num_args-1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gacde98ce4a8ed1dde50b9669db4838c61>
--
mkAnd :: Context -> [AST Bool] -> IO (AST Bool)
mkAnd _ [] = error "Z3.Base.mkAnd: empty list of expressions"
mkAnd c es 
  = withArray es $ \aptr -> 
    withForeignPtr (unContext c) $ \cptr ->
      AST <$> z3_mk_and cptr n (castPtr aptr) 
  where n = fromIntegral $ length es

-- | Create an AST node representing args[0] or ... or args[num_args-1]. 
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga00866d16331d505620a6c515302021f9>
--
mkOr :: Context -> [AST Bool] -> IO (AST Bool)
mkOr _ [] = error "Z3.Base.mkOr: empty list of expressions"
mkOr c es 
  = withArray es $ \aptr -> 
    withForeignPtr (unContext c) $ \cptr ->
      AST <$> z3_mk_or cptr n (castPtr aptr) 
  where n = fromIntegral $ length es

-- | Create an AST node representing args[0] + ... + args[num_args-1].  
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4e4ac0a4e53eee0b4b0ef159ed7d0cd5>
--
mkAdd :: Z3Num a => Context -> [AST a] -> IO (AST a)
mkAdd _ [] = error "Z3.Base.mkAdd: empty list of expressions"
mkAdd c es 
  = withArray es $ \aptr -> 
    withForeignPtr (unContext c) $ \cptr ->
      AST <$> z3_mk_add cptr n (castPtr aptr) 
  where n = fromIntegral $ length es

-- | Create an AST node representing args[0] * ... * args[num_args-1].  
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab9affbf8401a18eea474b59ad4adc890>
--
mkMul :: Z3Num a => Context -> [AST a] -> IO (AST a)
mkMul _ [] = error "Z3.Base.mkMul: empty list of expressions"
mkMul c es 
  = withArray es $ \aptr -> 
    withForeignPtr (unContext c) $ \cptr ->
      AST <$> z3_mk_mul cptr n (castPtr aptr) 
  where n = fromIntegral $ length es

-- | Create an AST node representing args[0] - ... - args[num_args - 1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4f5fea9b683f9e674fd8f14d676cc9a9>
--
mkSub ::Z3Num a => Context -> [AST a] -> IO (AST a)
mkSub _ [] = error "Z3.Base.mkSub: empty list of expressions"
mkSub c es 
  = withArray es $ \aptr -> 
    withForeignPtr (unContext c) $ \cptr ->
      AST <$> z3_mk_sub cptr n (castPtr aptr) 
  where n = fromIntegral $ length es

-- | Create an AST node representing -arg.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gadcd2929ad732937e25f34277ce4988ea>
--
mkUnaryMinus :: Z3Num a => Context -> AST a -> IO (AST a)
mkUnaryMinus c e = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_unary_minus cptr (unAST e)

-- | Create an AST node representing arg1 div arg2.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1ac60ee8307af8d0b900375914194ff3>
--
mkDiv :: Z3Num a => Context -> AST a -> AST a -> IO (AST a)
mkDiv c e1 e2 = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_div cptr (unAST e1) (unAST e2)

-- | Create an AST node representing arg1 mod arg2.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga8e350ac77e6b8fe805f57efe196e7713>
--
mkMod :: Z3Int a => Context -> AST a -> AST a -> IO (AST a)
mkMod c e1 e2 = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_mod cptr (unAST e1) (unAST e2)

-- | Create an AST node representing arg1 rem arg2.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga2fcdb17f9039bbdaddf8a30d037bd9ff>
--
mkRem :: Z3Int a => Context -> AST a -> AST a -> IO (AST a)
mkRem c e1 e2 = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_rem cptr (unAST e1) (unAST e2)

-- | Create less than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga58a3dc67c5de52cf599c346803ba1534>
--
mkLt :: Z3Num a => Context -> AST a -> AST a -> IO (AST Bool)
mkLt c e1 e2 = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_lt cptr (unAST e1) (unAST e2)

-- | Create less than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaa9a33d11096841f4e8c407f1578bc0bf>
--
mkLe :: Z3Num a => Context -> AST a -> AST a -> IO (AST Bool)
mkLe c e1 e2 = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_le cptr (unAST e1) (unAST e2)

-- | Create greater than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga46167b86067586bb742c0557d7babfd3>
--
mkGt :: Z3Num a => Context -> AST a -> AST a -> IO (AST Bool)
mkGt c e1 e2 = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_gt cptr (unAST e1) (unAST e2)

-- | Create greater than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gad9245cbadb80b192323d01a8360fb942>
--
mkGe :: Z3Num a => Context -> AST a -> AST a -> IO (AST Bool)
mkGe c e1 e2 = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_ge cptr (unAST e1) (unAST e2)

-- | Coerce an integer to a real.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga7130641e614c7ebafd28ae16a7681a21>
--
mkInt2Real :: (Z3Int a, Z3Real b) => Context -> AST a -> IO (AST b)
mkInt2Real c e = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_int2real cptr (unAST e)

-- | Coerce a real to an integer.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga759b6563ba1204aae55289009a3fdc6d>
--
mkReal2Int :: (Z3Real a, Z3Int b) => Context -> AST a -> IO (AST b)
mkReal2Int c e = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_real2int cptr (unAST e)

-- | Check if a real number is an integer.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaac2ad0fb04e4900fdb4add438d137ad3>
--
mkIsInt :: Z3Real a => Context -> AST a -> IO (AST Bool)
mkIsInt c e = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_is_int cptr (unAST e)

-- TODO Constants and applications: bitvectors and arrays
-- TODO Sets


---------------------------------------------------------------------
-- * Numerals

-- | Create a numeral of a given sort.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac8aca397e32ca33618d8024bff32948c>
--
mkNumeral :: Z3Num a => Context -> String -> Sort a -> IO (AST a)
mkNumeral c str s =
    withForeignPtr (unContext c) $ \cptr ->
    withCString str              $ \cstr->
        AST <$> z3_mk_numeral cptr cstr (unSort s)

-- | Create a numeral of sort /int/.
mkInt :: Z3Int a => Context -> a -> IO (AST a)
mkInt c n =
    mkIntSort c >>=
        mkNumeral c n_str
    where n_str = show $ toInteger n

-- | Create a numeral of sort /real/.
mkReal :: Z3Real r => Context -> r -> IO (AST r)
mkReal c n =
    mkRealSort c >>=
        mkNumeral c n_str
    where r = toRational n
          r_n = toInteger $ numerator r
          r_d = toInteger $ denominator r
          n_str =    show r_n ++ " / " ++ show r_d

{-# RULES "mkReal/mkRealZ3" mkReal = mkRealZ3 #-}
mkRealZ3 :: Z3Real a => Context -> Ratio Int32 -> IO (AST a)
mkRealZ3 c r =
    withForeignPtr (unContext c) $ \ctxPtr ->
        AST <$> z3_mk_real ctxPtr n d
    where n = (fromIntegral $ numerator r)   :: CInt
          d = (fromIntegral $ denominator r) :: CInt

{-# RULES "mkInt/mkInt_IntZ3" mkInt = mkInt_IntZ3 #-}
mkInt_IntZ3 :: Z3Int a => Context -> Int32 -> IO (AST a)
mkInt_IntZ3 c n = mkIntSort c >>= mkIntZ3 c n

{-# RULES "mkInt/mkInt_UnsignedIntZ3" mkInt = mkInt_UnsignedIntZ3 #-}
mkInt_UnsignedIntZ3 :: Z3Int a => Context -> Word32 -> IO (AST a)
mkInt_UnsignedIntZ3 c n = mkIntSort c >>= mkUnsignedIntZ3 c n

{-# RULES "mkInt/mkInt_Int64Z3" mkInt = mkInt_Int64Z3 #-}
mkInt_Int64Z3 :: Z3Int a => Context -> Int64 -> IO (AST a)
mkInt_Int64Z3 c n = mkIntSort c >>= mkInt64Z3 c n

{-# RULES "mkInt/mkInt_UnsignedInt64Z3" mkInt = mkInt_UnsignedInt64Z3 #-}
mkInt_UnsignedInt64Z3 :: Z3Int a => Context -> Word64 -> IO (AST a)
mkInt_UnsignedInt64Z3 c n = mkIntSort c >>= mkUnsignedInt64Z3 c n

{-# RULES "mkReal/mkReal_IntZ3" mkReal = mkReal_IntZ3 #-}
mkReal_IntZ3 :: Z3Real r => Context -> Int32 -> IO (AST r)
mkReal_IntZ3 c n = mkRealSort c >>= mkIntZ3 c n

{-# RULES "mkReal/mkReal_UnsignedIntZ3" mkReal = mkReal_UnsignedIntZ3 #-}
mkReal_UnsignedIntZ3 :: Z3Real r => Context -> Word32 -> IO (AST r)
mkReal_UnsignedIntZ3 c n = mkRealSort c >>= mkUnsignedIntZ3 c n

{-# RULES "mkReal/mkReal_Int64Z3" mkReal = mkReal_Int64Z3 #-}
mkReal_Int64Z3 :: Z3Real r => Context -> Int64 -> IO (AST r)
mkReal_Int64Z3 c n = mkRealSort c >>= mkInt64Z3 c n

{-# RULES "mkReal/mkReal_UnsignedInt64Z3" mkReal = mkReal_UnsignedInt64Z3 #-}
mkReal_UnsignedInt64Z3 :: Z3Real r => Context -> Word64 -> IO (AST r)
mkReal_UnsignedInt64Z3 c n = mkRealSort c >>= mkUnsignedInt64Z3 c n

{-# INLINE mkIntZ3 #-}
mkIntZ3 :: Z3Num a => Context -> Int32 -> Sort a -> IO (AST a)
mkIntZ3 c n s =
    withForeignPtr (unContext c) $ \ctxPtr ->
        AST <$> z3_mk_int ctxPtr cn (unSort s)
    where cn = (fromIntegral n) :: CInt

{-# INLINE mkUnsignedIntZ3 #-}
mkUnsignedIntZ3 :: Z3Num a => Context -> Word32 -> Sort a -> IO (AST a)
mkUnsignedIntZ3 c n s =
    withForeignPtr (unContext c) $ \ctxPtr ->
        AST <$> z3_mk_unsigned_int ctxPtr cn (unSort s)
    where cn = (fromIntegral n) :: CUInt

{-# INLINE mkInt64Z3 #-}
mkInt64Z3 :: Z3Num a => Context -> Int64 -> Sort a -> IO (AST a)
mkInt64Z3 c n s =
    withForeignPtr (unContext c) $ \ctxPtr ->
        AST <$> z3_mk_int64 ctxPtr cn (unSort s)
    where cn = (fromIntegral n) :: CLLong

{-# INLINE mkUnsignedInt64Z3 #-}
mkUnsignedInt64Z3 :: Z3Num a => Context -> Word64 -> Sort a -> IO (AST a)
mkUnsignedInt64Z3 c n s =
    withForeignPtr (unContext c) $ \ctxPtr ->
        AST <$> z3_mk_unsigned_int64 ctxPtr cn (unSort s)
    where cn = (fromIntegral n) :: CULLong

---------------------------------------------------------------------
-- * Constraints

-- TODO Constraints: Z3_push
-- TODO Constraints: Z3_pop
-- TODO Constraints: Z3_get_num_scopes
-- TODO Constraints: Z3_persist_ast

-- | Assert a constraing into the logical context.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1a05ff73a564ae7256a2257048a4680a>
--
assertCnstr :: Context -> AST a -> IO ()
assertCnstr ctx ast =
    withForeignPtr (unContext ctx) $ \ctxPtr ->
        z3_assert_cnstr ctxPtr (unAST ast)

-- TODO Constraints: Z3_check_and_get_model

-- | Check whether the given logical context is consistent or not. 
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga72055cfbae81bd174abed32a83e50b03>
--
check :: Context -> IO Result
check ctx =
    toResult <$>
        withForeignPtr (unContext ctx) z3_check

-- TODO Constraints: Z3_check_assumptions
-- TODO Constraints: Z3_get_implied_equalities
-- TODO Constraints: Z3_del_model

-- TODO From section 'Constraints' on.

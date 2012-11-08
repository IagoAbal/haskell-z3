{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module    : Z3.Base
-- Copyright : (c) Iago Abal, 2012
--             (c) David Castro, 2012
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>,
--             David Castro <david.castro.dcp@gmail.com>
--
-- Medium-level bindings, highly inspired by yices-painless.
--

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

    , castAST
    
    -- ** Satisfiability result
    , Result(..)

    -- ** Z3 types
    , Z3Type(..)
    , Z3Fun()
    , Z3Num

    -- * Configuration
    , mkConfig
    , setParamValue
    , set_MODEL
    , set_MODEL_PARTIAL
    , set_WELL_SORTED_CHECK

    -- * Context
    , mkContext

    -- * Symbols
    , mkStringSymbol

    -- * Sorts
    , mkBoolSort
    , mkIntSort
    , mkRealSort

    -- * Constants and Applications
    , mkFuncDecl
    , mkApp1, mkApp2, mkApp3, mkApp4, mkApp5, mkApp6
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

    -- * Quantifiers
    , mkPattern
    , mkBound
    , mkForall

    -- * Accessors
    , getBool
    , getInt
    , getReal

    -- * Models
    , eval

    -- * Constraints
    , assertCnstr
    , check
    , getModel

    ) where

import Z3.Base.C
import Z3.Lang.TY

import Control.Applicative ( (<$>) )
import Data.Int
import Data.Ratio ( Ratio, numerator, denominator, (%) )
import Data.Typeable ( Typeable, typeOf )
import Data.Word
import Foreign hiding ( newForeignPtr, addForeignPtrFinalizer, toBool )
import Foreign.C
  ( CInt, CUInt, CLLong, CULLong
  , peekCString
  , withCString )
import Foreign.Concurrent ( newForeignPtr, addForeignPtrFinalizer )

---------------------------------------------------------------------
-- Types
--

-- | A Z3 /configuration object/.
-- 
--
-- /Notes:/
--
-- * The resource is automatically managed by the Haskell garbage
-- collector, and the structure is automatically deleted once it is out
-- of scope (no need to call 'z3_del_config').
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
newtype Context = Context { unContext :: ForeignPtr Z3_context }
    deriving Eq

-- | A Z3 /Lisp-link symbol/.
-- 
newtype Symbol = Symbol { unSymbol :: Ptr Z3_symbol }
    deriving (Eq, Ord, Show, Storable)

-- | A Z3 /AST node/.
-- 
-- TODO: Does the extra type safety provided by the phantom type worth
--       complicating the higher-level layers such as 'Z3.Monad' ?
--
newtype AST a = AST { unAST :: Ptr Z3_ast }
    deriving (Eq, Ord, Show, Storable, Typeable)

-- | Cast an 'AST a' to 'AST b' when 'a' and 'b' are the same type.
--
-- This is useful when unpacking an existentially quantified AST.
--
castAST :: forall a b. (Z3Type a, Z3Type b) => AST a -> Maybe (AST b)
castAST (AST a) 
    | typeOf (TY::TY a) == typeOf (TY::TY b) = Just (AST a)
    | otherwise                              = Nothing

-- | Kind of Z3 AST representing /types/.
--
newtype Sort a = Sort { unSort :: Ptr Z3_sort }
    deriving (Eq, Ord, Show, Storable)

-- | Kind of AST used to represent function symbols.
--
newtype FuncDecl a = FuncDecl { unFuncDecl :: Ptr Z3_func_decl }
    deriving (Eq, Ord, Show, Storable, Typeable)

-- | A kind of Z3 AST used to represent constant and function declarations.
--
newtype App a = App { _unApp :: Ptr Z3_app }
    deriving (Eq, Ord, Show, Storable)

-- | A kind of AST used to represent pattern and multi-patterns used to 
--   guide quantifier instantiation.
--
newtype Pattern = Pattern { _unPattern :: Ptr Z3_pattern }
    deriving (Eq, Ord, Show, Storable)

-- | A model for the constraints asserted into the logical context.
--
newtype Model = Model { unModel :: Ptr Z3_model }
    deriving Eq

-- | Result of a satisfiability check.
--
data Result a
    = Sat a
    | Unsat
    | Undef
    deriving (Eq, Ord, Read, Show)

-- | Convert 'Z3_lbool' from Z3.Base.C to 'Result'
--
toResult :: Z3_lbool -> Result ()
toResult lb
    | lb == z3_l_true  = Sat ()
    | lb == z3_l_false = Unsat
    | lb == z3_l_undef = Undef
    | otherwise        = error "Z3.Base.toResult: illegal `Z3_lbool' value"

-- | Convert 'Z3_bool' to 'Bool'.
--
-- 'Foreign.toBool' should be OK but this is convenient.
--
toBool :: Z3_bool -> Bool
toBool b
    | b == z3_true  = True
    | b == z3_false = False
    | otherwise     = error "Z3.Base.toBool: illegal `Z3_bool' value"

-----------------------------------------------------------
-- Z3 types
--

-- | A Z3 type
--
class (Eq a, Show a, Typeable a) => Z3Type a where
  mkSort :: Context -> IO (Sort a)
  mkValue :: Context -> a -> IO (AST a)
  getValue :: Context -> AST a -> IO a

instance Z3Type Bool where
  mkSort = mkBoolSort
  mkValue ctx True = mkTrue ctx
  mkValue ctx False = mkFalse ctx
  getValue ctx a = maybe False id <$> getBool ctx a

instance Z3Type Integer where
  mkSort = mkIntSort
  mkValue = mkInt
  getValue = getInt

instance Z3Type Rational where
  mkSort = mkRealSort
  mkValue = mkReal
  getValue = getReal

-- | A Function type
--
class Z3Fun a where
  -- Private functions: domain, range
  domain :: Context -> TY a -> IO (CUInt, [Ptr Z3_sort])
  range  :: Context -> TY a -> IO (Ptr Z3_sort)

instance (Z3Type a, Z3Type b) => Z3Fun (a -> b) where
  domain ctx _ = (1,) . (: []) . unSort <$> (mkSort ctx :: IO (Sort a))
  range  ctx _ = unSort <$> (mkSort ctx :: IO (Sort b))

instance (Z3Type a, Z3Fun (b -> c)) => Z3Fun (a -> b -> c) where
  domain ctx _ = do
    (srt1 :: Sort a) <- mkSort ctx
    (n,lst)          <- domain ctx (TY :: TY  (b -> c))
    return (n + 1, unSort srt1 : lst)
  range ctx _ = range ctx (TY :: TY (b -> c))


-- | A Z3 numeric type
--
class (Z3Type a, Num a) => Z3Num a where
instance Z3Num Integer where
instance Z3Num Rational where

---------------------------------------------------------------------
-- Configuration

-- | Create a configuration.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga7d6c40d9b79fe8a8851cc8540970787f>
--
mkConfig :: IO Config
mkConfig = do
  ptr <- z3_mk_config
  fptr <- newForeignPtr ptr (z3_del_config ptr)
  return $! Config fptr

-- | Set a configuration parameter.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga001ade87a1671fe77d7e53ed0f4f1ec3>
--
-- See: <http://research.microsoft.com/en-us/um/redmond/projects/z3/config.html>
--
setParamValue :: Config -> String -> String -> IO ()
setParamValue cfg s1 s2 =
    withForeignPtr (unConfig cfg) $ \cfgPtr ->
    withCString s1                $ \cs1 ->
    withCString s2                $ \cs2 ->
        z3_set_param_value cfgPtr cs1 cs2

-- | Set the /MODEL/ configuration parameter.
--
-- default: 'True', enable/disable model construction.
--
set_MODEL :: Config -> Bool -> IO ()
set_MODEL cfg True  = setParamValue cfg "MODEL" "true"
set_MODEL cfg False = setParamValue cfg "MODEL" "false"

-- | Set the /MODEL_PARTIAL/ configuration parameter.
--
-- default: 'False', enable/disable partial function interpretations.
--
set_MODEL_PARTIAL :: Config -> Bool -> IO ()
set_MODEL_PARTIAL cfg True  = setParamValue cfg "MODEL_PARTIAL" "true"
set_MODEL_PARTIAL cfg False = setParamValue cfg "MODEL_PARTIAL" "false"

-- | Set the /WELL_SORTED_CHECK/ configuration parameter.
--
-- default: 'True', enable/disable type checker.
--
set_WELL_SORTED_CHECK :: Config -> Bool -> IO ()
set_WELL_SORTED_CHECK cfg True  = setParamValue cfg "WELL_SORTED_CHECK" "true"
set_WELL_SORTED_CHECK cfg False = setParamValue cfg "WELL_SORTED_CHECK" "false"

---------------------------------------------------------------------
-- Context

-- | Create a context using the given configuration.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0bd93cfab4d749dd3e2f2a4416820a46>
--
mkContext :: Config -> IO Context
mkContext cfg = withForeignPtr (unConfig cfg) $ \cfgPtr -> do
  ptr <- z3_mk_context cfgPtr
  fptr <- newForeignPtr ptr (z3_del_context ptr)
  return $! Context fptr

---------------------------------------------------------------------
-- Symbols

-- | Create a Z3 symbol using a string.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gafebb0d3c212927cf7834c3a20a84ecae>
--
mkStringSymbol :: Context -> String -> IO Symbol
mkStringSymbol ctx s =
    withForeignPtr (unContext ctx) $ \ctxPtr ->
    withCString s                  $ \cs     ->
        Symbol <$> z3_mk_string_symbol ctxPtr cs

---------------------------------------------------------------------
-- Sorts

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
mkIntSort :: Context -> IO (Sort Integer)
mkIntSort c = withForeignPtr (unContext c) $ \cptr ->
  Sort <$> z3_mk_int_sort cptr

-- | Create a real type.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga40ef93b9738485caed6dc84631c3c1a0>
--
mkRealSort :: Context -> IO (Sort Rational)
mkRealSort c = withForeignPtr (unContext c) $ \cptr ->
  Sort <$> z3_mk_real_sort cptr

-- TODO Sorts: from Z3_mk_real_sort on

---------------------------------------------------------------------
-- Constants and Applications

-- | A Z3 function
mkFuncDecl :: forall t. Z3Fun t => Context -> Symbol -> IO (FuncDecl t)
mkFuncDecl ctx (Symbol smb) = withForeignPtr (unContext ctx) $ \ctxPtr -> do
  (len, dom) <- domain ctx (TY :: TY t)
  rng <- range  ctx (TY :: TY t)
  withArray dom $ \c_dom ->
    FuncDecl <$> z3_mk_func_decl ctxPtr smb len c_dom rng

-- | Create a constant or function application.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga33a202d86bf628bfab9b6f437536cebe>
--
mkApp1 :: (Z3Type a, Z3Type b)
            => Context
                -> FuncDecl (a -> b)
                -> AST a
                -> IO (AST b)
mkApp1 ctx fd a
  = AST <$> mkApp ctx fd [unAST a]

mkApp2 :: (Z3Type a, Z3Type b, Z3Type c)
            => Context
                -> FuncDecl (a -> b -> c)
                -> AST a -> AST b
                -> IO (AST c)
mkApp2 ctx fd a b
  = AST <$> mkApp ctx fd [unAST a,unAST b]

mkApp3 :: (Z3Type a, Z3Type b, Z3Type c , Z3Type d)
            => Context
                -> FuncDecl (a -> b -> c -> d)
                -> AST a -> AST b -> AST c
                -> IO (AST d)
mkApp3 ctx fd a b c
  = AST <$> mkApp ctx fd [unAST a,unAST b,unAST c]

mkApp4 :: (Z3Type a, Z3Type b, Z3Type c , Z3Type d, Z3Type e)
            => Context
                -> FuncDecl (a -> b -> c -> d -> e)
                -> AST a -> AST b -> AST c -> AST d
                -> IO (AST e)
mkApp4 ctx fd a b c d
  = AST <$> mkApp ctx fd [unAST a,unAST b,unAST c,unAST d]

mkApp5 :: (Z3Type a, Z3Type b, Z3Type c , Z3Type d, Z3Type e, Z3Type f)
            => Context
                -> FuncDecl (a -> b -> c -> d -> e -> f)
                -> AST a -> AST b -> AST c -> AST d -> AST e
                -> IO (AST f)
mkApp5 ctx fd a b c d e
  = AST <$> mkApp ctx fd [unAST a,unAST b ,unAST c,unAST d ,unAST e]

mkApp6 :: (Z3Type a, Z3Type b, Z3Type c , Z3Type d, Z3Type e, Z3Type f, Z3Type g)
            => Context
                -> FuncDecl (a -> b -> c -> d -> e -> f -> g)
                -> AST a -> AST b -> AST c -> AST d -> AST e -> AST f
                -> IO (AST g)
mkApp6 ctx fd a b c d e f
  = AST <$> mkApp ctx fd [unAST a,unAST b,unAST c,unAST d,unAST e,unAST f]


mkApp :: Context -> FuncDecl t -> [Ptr Z3_ast] -> IO (Ptr Z3_ast)
mkApp ctx fd args =
  withForeignPtr (unContext ctx) $ \ctxPtr ->
    withArray args $ \pargs ->
       z3_mk_app ctxPtr (unFuncDecl fd) (fromIntegral $ length args) pargs


-- | Declare and create a constant.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga093c9703393f33ae282ec5e8729354ef>
--
mkConst :: Z3Type a => Context -> Symbol -> Sort a -> IO (AST a)
mkConst c x s = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_const cptr (unSymbol x) (unSort s)

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
mkEq :: Z3Type a => Context -> AST a -> AST a -> IO (AST Bool)
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
mkMod :: Context -> AST Integer -> AST Integer -> IO (AST Integer)
mkMod c e1 e2 = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_mod cptr (unAST e1) (unAST e2)

-- | Create an AST node representing arg1 rem arg2.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga2fcdb17f9039bbdaddf8a30d037bd9ff>
--
mkRem :: Context -> AST Integer -> AST Integer -> IO (AST Integer)
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
mkInt2Real :: Context -> AST Integer -> IO (AST Rational)
mkInt2Real c e = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_int2real cptr (unAST e)

-- | Coerce a real to an integer.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga759b6563ba1204aae55289009a3fdc6d>
--
mkReal2Int :: Context -> AST Rational -> IO (AST Integer)
mkReal2Int c e = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_real2int cptr (unAST e)

-- | Check if a real number is an integer.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaac2ad0fb04e4900fdb4add438d137ad3>
--
mkIsInt :: Context -> AST Rational -> IO (AST Bool)
mkIsInt c e = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_is_int cptr (unAST e)

-- TODO Bit-vector, Arrays, Sets


---------------------------------------------------------------------
-- Numerals

-- | Create a numeral of a given sort.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac8aca397e32ca33618d8024bff32948c>
--
mkNumeral :: Z3Num a => Context -> String -> Sort a -> IO (AST a)
mkNumeral c str s =
    withForeignPtr (unContext c) $ \cptr ->
    withCString str              $ \cstr->
        AST <$> z3_mk_numeral cptr cstr (unSort s)

-------------------------------------------------
-- Numerals / Integers

-- | Create a numeral of sort /int/.
mkInt :: Integral a => Context -> a -> IO (AST Integer)
mkInt c n = mkIntSort c >>= mkNumeral c n_str
    where n_str = show $ toInteger n

{-# INLINE mkIntZ3 #-}
mkIntZ3 :: Z3Num a => Context -> Int32 -> Sort a -> IO (AST a)
mkIntZ3 c n s =
    withForeignPtr (unContext c) $ \ctxPtr ->
        AST <$> z3_mk_int ctxPtr cn (unSort s)
    where cn = fromIntegral n :: CInt

{-# INLINE mkUnsignedIntZ3 #-}
mkUnsignedIntZ3 :: Z3Num a => Context -> Word32 -> Sort a -> IO (AST a)
mkUnsignedIntZ3 c n s =
    withForeignPtr (unContext c) $ \ctxPtr ->
        AST <$> z3_mk_unsigned_int ctxPtr cn (unSort s)
    where cn = fromIntegral n :: CUInt

{-# INLINE mkInt64Z3 #-}
mkInt64Z3 :: Z3Num a => Context -> Int64 -> Sort a -> IO (AST a)
mkInt64Z3 c n s =
    withForeignPtr (unContext c) $ \ctxPtr ->
        AST <$> z3_mk_int64 ctxPtr cn (unSort s)
    where cn = fromIntegral n :: CLLong

{-# INLINE mkUnsignedInt64Z3 #-}
mkUnsignedInt64Z3 :: Z3Num a => Context -> Word64 -> Sort a -> IO (AST a)
mkUnsignedInt64Z3 c n s =
    withForeignPtr (unContext c) $ \ctxPtr ->
        AST <$> z3_mk_unsigned_int64 ctxPtr cn (unSort s)
    where cn = fromIntegral n :: CULLong

{-# RULES "mkInt/mkInt_IntZ3" mkInt = mkInt_IntZ3 #-}
mkInt_IntZ3 :: Context -> Int32 -> IO (AST Integer)
mkInt_IntZ3 c n = mkIntSort c >>= mkIntZ3 c n

{-# RULES "mkInt/mkInt_UnsignedIntZ3" mkInt = mkInt_UnsignedIntZ3 #-}
mkInt_UnsignedIntZ3 :: Context -> Word32 -> IO (AST Integer)
mkInt_UnsignedIntZ3 c n = mkIntSort c >>= mkUnsignedIntZ3 c n

{-# RULES "mkInt/mkInt_Int64Z3" mkInt = mkInt_Int64Z3 #-}
mkInt_Int64Z3 :: Context -> Int64 -> IO (AST Integer)
mkInt_Int64Z3 c n = mkIntSort c >>= mkInt64Z3 c n

{-# RULES "mkInt/mkInt_UnsignedInt64Z3" mkInt = mkInt_UnsignedInt64Z3 #-}
mkInt_UnsignedInt64Z3 :: Context -> Word64 -> IO (AST Integer)
mkInt_UnsignedInt64Z3 c n = mkIntSort c >>= mkUnsignedInt64Z3 c n

-------------------------------------------------
-- Numerals / Reals

-- | Create a numeral of sort /real/.
mkReal :: Real r => Context -> r -> IO (AST Rational)
mkReal c n = mkRealSort c >>= mkNumeral c n_str
    where r = toRational n
          r_n = toInteger $ numerator r
          r_d = toInteger $ denominator r
          n_str = show r_n ++ " / " ++ show r_d

{-# RULES "mkReal/mkRealZ3" mkReal = mkRealZ3 #-}
mkRealZ3 :: Context -> Ratio Int32 -> IO (AST Rational)
mkRealZ3 c r =
    withForeignPtr (unContext c) $ \ctxPtr ->
        AST <$> z3_mk_real ctxPtr n d
    where n = (fromIntegral $ numerator r)   :: CInt
          d = (fromIntegral $ denominator r) :: CInt

{-# RULES "mkReal/mkReal_IntZ3" mkReal = mkReal_IntZ3 #-}
mkReal_IntZ3 :: Context -> Int32 -> IO (AST Rational)
mkReal_IntZ3 c n = mkRealSort c >>= mkIntZ3 c n

{-# RULES "mkReal/mkReal_UnsignedIntZ3" mkReal = mkReal_UnsignedIntZ3 #-}
mkReal_UnsignedIntZ3 :: Context -> Word32 -> IO (AST Rational)
mkReal_UnsignedIntZ3 c n = mkRealSort c >>= mkUnsignedIntZ3 c n

{-# RULES "mkReal/mkReal_Int64Z3" mkReal = mkReal_Int64Z3 #-}
mkReal_Int64Z3 :: Context -> Int64 -> IO (AST Rational)
mkReal_Int64Z3 c n = mkRealSort c >>= mkInt64Z3 c n

{-# RULES "mkReal/mkReal_UnsignedInt64Z3" mkReal = mkReal_UnsignedInt64Z3 #-}
mkReal_UnsignedInt64Z3 :: Context -> Word64 -> IO (AST Rational)
mkReal_UnsignedInt64Z3 c n = mkRealSort c >>= mkUnsignedInt64Z3 c n

---------------------------------------------------------------------
-- Quantifiers

mkPattern :: Context -> [AST a] -> IO Pattern
mkPattern _ [] = error "Z3.Base.mkPattern: empty list of expressions"
mkPattern c es
  = withArray es $ \aptr ->
    withForeignPtr (unContext c) $ \cptr ->
      Pattern <$> z3_mk_pattern cptr n (castPtr aptr)
  where n = fromIntegral $ length es

mkBound :: Context -> Int -> Sort a -> IO (AST a)
mkBound c i s
  | i >= 0    = withForeignPtr (unContext c) $ \cptr ->
                  AST <$> z3_mk_bound cptr (fromIntegral i) (unSort s)
  | otherwise = error "Z3.Base.mkBound: negative de-Bruijn index"

mkForall :: Context -> [Pattern] -> Symbol -> Sort a -> AST Bool -> IO (AST Bool)
mkForall c pats x s p
  = withArray pats $ \patsPtr ->
    with (unSymbol x) $ \xptr ->
    with (unSort s)   $ \sptr ->
    withForeignPtr (unContext c) $ \cptr ->
      AST <$> z3_mk_forall cptr 0 n (castPtr patsPtr) 1 sptr xptr (unAST p)
  where n = fromIntegral $ length pats

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
--
getBool :: Context -> AST Bool -> IO (Maybe Bool)
getBool c a = withForeignPtr (unContext c) $ \ctxPtr ->
  castLBool <$> z3_get_bool_value ctxPtr (unAST a)

-- | Return numeral value, as a string of a numeric constant term.  
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga94617ef18fa7157e1a3f85db625d2f4b>
--
getNumeralString :: Z3Num a => Context -> AST a -> IO String
getNumeralString c a = withForeignPtr (unContext c) $ \ctxPtr ->
  peekCString =<< z3_get_numeral_string ctxPtr (unAST a)

-- | Return 'Z3Int' value
--
getInt :: Context -> AST Integer -> IO Integer
getInt c a = read <$> getNumeralString c a

-- | Return 'Z3Real' value
--
getReal :: Context -> AST Rational -> IO Rational
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

mkModel :: Context -> Ptr Z3_model -> IO Model
mkModel ctx ptr = withForeignPtr fptr $ \ctxPtr -> do
    addForeignPtrFinalizer fptr $ z3_del_model ctxPtr ptr
    return $ Model ptr
  where fptr = unContext ctx

-- | Evaluate an AST node in the given model.
eval :: Context -> Model -> AST a -> IO (Maybe (AST a))
eval ctx m a
  = withForeignPtr (unContext ctx) $ \ctxPtr ->
      alloca $ \aptr2 ->
        z3_eval ctxPtr (unModel m) (unAST a) aptr2 >>= peekAST aptr2 . toBool
  where peekAST :: Ptr (Ptr Z3_ast) -> Bool -> IO (Maybe (AST a))
        peekAST _p False = return Nothing
        peekAST  p True  = Just . AST <$> peek p

---------------------------------------------------------------------
-- Constraints

-- TODO Constraints: Z3_push
-- TODO Constraints: Z3_pop
-- TODO Constraints: Z3_get_num_scopes
-- TODO Constraints: Z3_persist_ast

-- | Assert a constraing into the logical context.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1a05ff73a564ae7256a2257048a4680a>
--
assertCnstr :: Context -> AST Bool -> IO ()
assertCnstr ctx ast =
    withForeignPtr (unContext ctx) $ \ctxPtr ->
        z3_assert_cnstr ctxPtr (unAST ast)

-- | Get model (logical context is consistent)
--
-- Reference : <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaff310fef80ac8a82d0a51417e073ec0a>
--
getModel :: Context -> IO (Result Model)
getModel c = withForeignPtr (unContext c) $ \ctxPtr ->
  alloca $ \mptr ->
    z3_check_and_get_model ctxPtr mptr >>= peekModel mptr . toResult
  where peekModel :: Ptr (Ptr Z3_model)
                  -> Result ()
                  -> IO (Result Model)
        peekModel _ Unsat                   = return Unsat
        peekModel _ Undef                   = return Undef
        peekModel p (Sat ()) | p == nullPtr = error "Z3.Base.getModel: Panic! nullPtr!"
                             | otherwise    = do z3m <- peek p
                                                 m <- mkModel c z3m
                                                 return $ Sat m

-- | Check whether the given logical context is consistent or not. 
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga72055cfbae81bd174abed32a83e50b03>
--
check :: Context -> IO (Result ())
check ctx = toResult <$> withForeignPtr (unContext ctx) z3_check

-- TODO Constraints: Z3_check_assumptions
-- TODO Constraints: Z3_get_implied_equalities

-- TODO From section 'Constraints' on.

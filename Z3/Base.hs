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

    ) where

import Z3.Base.C

import Control.Applicative ( (<$>) )
import Foreign

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
newtype Config = Config { _unConfig :: ForeignPtr Z3_config }
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
newtype AST = AST { unAST :: Ptr Z3_ast }
    deriving (Eq, Ord, Show, Storable)

    -- TODO Improve type-safety with phantom types.

-- | Kind of Z3 AST representing /types/.
-- 
-- /Reference:/ < TODO >
--
newtype Sort = Sort { unSort :: Ptr Z3_sort }
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
mkConst :: Context -> Symbol -> Sort -> IO AST
mkConst c x s = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_const cptr (unSymbol x) (unSort s)

-- TODO Constants and Applications: Z3_mk_label
-- TODO Constants and Applications: Z3_mk_fresh_func_decl
-- TODO Constants and Applications: Z3_mk_fresh_const

-- | Create an AST node representing /true/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae898e7380409bbc57b56cc5205ef1db7>
--
mkTrue :: Context -> IO AST
mkTrue c = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_true cptr

-- | Create an AST node representing /false/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5952ac17671117a02001fed6575c778d>
--
mkFalse :: Context -> IO AST
mkFalse c = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_false cptr

-- | Create an AST node representing l = r.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga95a19ce675b70e22bb0401f7137af37c>
--
mkEq :: Context -> AST -> AST -> IO AST
mkEq c e1 e2 = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_eq cptr (unAST e1) (unAST e2)

-- | Create an AST node representing not(a).
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga3329538091996eb7b3dc677760a61072>
--
mkNot :: Context -> AST -> IO AST
mkNot c e = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_not cptr (unAST e)

-- | Create an AST node representing an if-then-else: ite(t1, t2, t3).
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga94417eed5c36e1ad48bcfc8ad6e83547>
--
mkIte :: Context -> AST -> AST -> AST -> IO AST
mkIte c g e1 e2 = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_ite cptr (unAST g) (unAST e1) (unAST e2)

-- | Create an AST node representing t1 iff t2. 
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga930a8e844d345fbebc498ac43a696042>
--
mkIff :: Context -> AST -> AST -> IO AST
mkIff c p q = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_iff cptr (unAST p) (unAST q)

-- | Create an AST node representing t1 implies t2. 
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac829c0e25bbbd30343bf073f7b524517>
--
mkImplies :: Context -> AST -> AST -> IO AST
mkImplies c p q = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_implies cptr (unAST p) (unAST q)

-- | Create an AST node representing t1 xor t2. 
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gacc6d1b848032dec0c4617b594d4229ec>
--
mkXor :: Context -> AST -> AST -> IO AST
mkXor c p q = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_xor cptr (unAST p) (unAST q)

-- | Create an AST node representing args[0] and ... and args[num_args-1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gacde98ce4a8ed1dde50b9669db4838c61>
--
mkAnd :: Context -> [AST] -> IO AST
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
mkOr :: Context -> [AST] -> IO AST
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
mkAdd :: Context -> [AST] -> IO AST
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
mkMul :: Context -> [AST] -> IO AST
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
mkSub :: Context -> [AST] -> IO AST
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
mkUnaryMinus :: Context -> AST -> IO AST
mkUnaryMinus c e = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_unary_minus cptr (unAST e)

-- | Create an AST node representing arg1 div arg2.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1ac60ee8307af8d0b900375914194ff3>
--
mkDiv :: Context -> AST -> AST -> IO AST
mkDiv c e1 e2 = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_div cptr (unAST e1) (unAST e2)

-- | Create an AST node representing arg1 mod arg2.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga8e350ac77e6b8fe805f57efe196e7713>
--
mkMod :: Context -> AST -> AST -> IO AST
mkMod c e1 e2 = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_mod cptr (unAST e1) (unAST e2)

-- | Create an AST node representing arg1 rem arg2.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga2fcdb17f9039bbdaddf8a30d037bd9ff>
--
mkRem :: Context -> AST -> AST -> IO AST
mkRem c e1 e2 = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_rem cptr (unAST e1) (unAST e2)

-- | Create less than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga58a3dc67c5de52cf599c346803ba1534>
--
mkLt :: Context -> AST -> AST -> IO AST
mkLt c e1 e2 = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_lt cptr (unAST e1) (unAST e2)

-- | Create less than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaa9a33d11096841f4e8c407f1578bc0bf>
--
mkLe :: Context -> AST -> AST -> IO AST
mkLe c e1 e2 = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_le cptr (unAST e1) (unAST e2)

-- | Create greater than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga46167b86067586bb742c0557d7babfd3>
--
mkGt :: Context -> AST -> AST -> IO AST
mkGt c e1 e2 = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_gt cptr (unAST e1) (unAST e2)

-- | Create greater than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gad9245cbadb80b192323d01a8360fb942>
--
mkGe :: Context -> AST -> AST -> IO AST
mkGe c e1 e2 = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_ge cptr (unAST e1) (unAST e2)

-- | Coerce an integer to a real.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga7130641e614c7ebafd28ae16a7681a21>
--
mkInt2Real :: Context -> AST -> IO AST
mkInt2Real c e = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_int2real cptr (unAST e)

-- | Coerce a real to an integer.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga759b6563ba1204aae55289009a3fdc6d>
--
mkReal2Int :: Context -> AST -> IO AST
mkReal2Int c e = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_real2int cptr (unAST e)

-- | Check if a real number is an integer.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaac2ad0fb04e4900fdb4add438d137ad3>
--
mkIsInt :: Context -> AST -> IO AST
mkIsInt c e = withForeignPtr (unContext c) $ \cptr ->
  AST <$> z3_mk_is_int cptr (unAST e)

-- TODO Constants and applications: bitvectors and arrays
-- TODO Sets
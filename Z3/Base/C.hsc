{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE ForeignFunctionInterface   #-}

-- |
-- Module    : Z3.Base.C
-- Copyright : (c) Iago Abal, 2012-2013
--             (c) David Castro, 2012-2013
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>,
--             David Castro <david.castro.dcp@gmail.com>
--
-- Low-level bindings, highly inspired by yices-painless.
--


module Z3.Base.C where

import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <z3.h>


---------------------------------------------------------------------
-- * Types

-- | A configuration object used to initialize logical contexts.
data Z3_config

-- | Logical context. This is the main Z3 data-structure.
data Z3_context

-- | A Lisp-link symbol. It is used to name types, constants, and functions.
-- A symbol can be created using string or integers.
data Z3_symbol

-- | Abstract syntax tree node. That is, the data-structure used in Z3 to
-- represent terms, formulas and types.
data Z3_ast

-- | A kind of AST used to represent types.
data Z3_sort

-- | A kind of AST used to represent function symbols.
data Z3_func_decl

-- | A kind of AST used to represent constant and function declarations.
data Z3_app

-- | A kind of AST used to represent pattern and multi-patterns used to
-- guide quantifier instantiation.
data Z3_pattern

-- | A model for the constraints asserted into the logical context.
data Z3_model

-- | Lifted Boolean type: false, undefined, true.
type Z3_lbool = CInt

-- | Values of lifted boolean type
z3_l_true, z3_l_false, z3_l_undef :: Z3_lbool
z3_l_true  = #const Z3_L_TRUE
z3_l_false = #const Z3_L_FALSE
z3_l_undef = #const Z3_L_UNDEF

-- | Boolean type. It is just an alias for int.
type Z3_bool = CInt

-- | Z3_bool values
z3_true, z3_false :: Z3_lbool
z3_true  = #const Z3_TRUE
z3_false = #const Z3_FALSE

-- | Z3 String type
type Z3_string = CString

---------------------------------------------------------------------
-- * Create configuration

-- | Create a configuration.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga7d6c40d9b79fe8a8851cc8540970787f>
--
foreign import ccall unsafe "Z3_mk_config"
    z3_mk_config :: IO (Ptr Z3_config)

-- | Delete the given configuration object.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5e620acf5d55d0271097c9bb97219774>
--
foreign import ccall unsafe "Z3_del_config"
    z3_del_config :: Ptr Z3_config -> IO ()

-- | Set a configuration parameter.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga001ade87a1671fe77d7e53ed0f4f1ec3>
--
foreign import ccall unsafe "Z3_set_param_value"
    z3_set_param_value :: Ptr Z3_config -> Z3_string -> Z3_string -> IO ()


---------------------------------------------------------------------
-- * Create context

-- | Create a context using the given configuration.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0bd93cfab4d749dd3e2f2a4416820a46>
--
foreign import ccall unsafe "Z3_mk_context"
    z3_mk_context :: Ptr Z3_config -> IO (Ptr Z3_context)

-- | Delete the given logical context.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga556eae80ed43ab13e1e7dc3b38c35200>
--
foreign import ccall unsafe "Z3_del_context"
    z3_del_context :: Ptr Z3_context -> IO ()


---------------------------------------------------------------------
-- * Symbols

-- | Create a Z3 symbol using a C string.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gafebb0d3c212927cf7834c3a20a84ecae>
--
foreign import ccall unsafe "Z3_mk_string_symbol"
    z3_mk_string_symbol :: Ptr Z3_context -> Z3_string -> IO (Ptr Z3_symbol)


---------------------------------------------------------------------
-- * Sorts

-- TODO Sorts: Z3_is_eq_sort
-- TODO Sorts: Z3_mk_uninterpreted_sort

-- | Create the Boolean type.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gacdc73510b69a010b71793d429015f342>
--
foreign import ccall unsafe "Z3_mk_bool_sort"
    z3_mk_bool_sort :: Ptr Z3_context -> IO (Ptr Z3_sort)

-- | Create an integer type.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga6cd426ab5748653b77d389fd3eac1015>
--
foreign import ccall unsafe "Z3_mk_int_sort"
    z3_mk_int_sort :: Ptr Z3_context -> IO (Ptr Z3_sort)

-- | Create a real type.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga40ef93b9738485caed6dc84631c3c1a0>
--
foreign import ccall unsafe "Z3_mk_real_sort"
    z3_mk_real_sort :: Ptr Z3_context -> IO (Ptr Z3_sort)

-- | Create a bit-vector type of the given size.
--
-- This type can also be seen as a machine integer.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaeed000a1bbb84b6ca6fdaac6cf0c1688>
--
foreign import ccall unsafe "Z3_mk_bv_sort"
    z3_mk_bv_sort :: Ptr Z3_context -> CUInt -> IO (Ptr Z3_sort)

-- TODO Sorts: from Z3_mk_bv_sort on


---------------------------------------------------------------------
-- * Constants and Applications

-- | Declare a constant or function.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaa5c5e2602a44d5f1373f077434859ca2>
--
foreign import ccall unsafe "Z3_mk_func_decl"
    z3_mk_func_decl :: Ptr Z3_context
                         -> Ptr Z3_symbol
                         -> CUInt
                         -> Ptr (Ptr Z3_sort)
                         -> Ptr Z3_sort
                         -> IO (Ptr Z3_func_decl)

-- | Create a constant or function application.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga33a202d86bf628bfab9b6f437536cebe>
--
foreign import ccall unsafe "Z3_mk_app"
    z3_mk_app :: Ptr Z3_context
                   -> Ptr Z3_func_decl
                   -> CUInt
                   -> Ptr (Ptr Z3_ast)
                   -> IO (Ptr Z3_ast)

-- | Declare and create a constant.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga093c9703393f33ae282ec5e8729354ef>
--
foreign import ccall unsafe "Z3_mk_const"
    z3_mk_const :: Ptr Z3_context -> Ptr Z3_symbol -> Ptr Z3_sort -> IO (Ptr Z3_ast)

-- TODO Constants and Applications: Z3_mk_fresh_func_decl
-- TODO Constants and Applications: Z3_mk_fresh_const

---------------------------------------------------------------------
-- * Propositional Logic and Equality

-- | Create an AST node representing /true/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae898e7380409bbc57b56cc5205ef1db7>
--
foreign import ccall unsafe "Z3_mk_true"
    z3_mk_true :: Ptr Z3_context -> IO (Ptr Z3_ast)

-- | Create an AST node representing /false/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5952ac17671117a02001fed6575c778d>
--
foreign import ccall unsafe "Z3_mk_false"
    z3_mk_false :: Ptr Z3_context -> IO (Ptr Z3_ast)

-- | Create an AST node representing l = r.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga95a19ce675b70e22bb0401f7137af37c>
--
foreign import ccall unsafe "Z3_mk_eq"
    z3_mk_eq :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- TODO: Z3_mk_distinct

-- | Create an AST node representing not(a).
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga3329538091996eb7b3dc677760a61072>
--
foreign import ccall unsafe "Z3_mk_not"
    z3_mk_not :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Create an AST node representing an if-then-else: ite(t1, t2, t3).
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga94417eed5c36e1ad48bcfc8ad6e83547>
--
foreign import ccall unsafe "Z3_mk_ite"
    z3_mk_ite :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Create an AST node representing t1 iff t2.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga930a8e844d345fbebc498ac43a696042>
--
foreign import ccall unsafe "Z3_mk_iff"
    z3_mk_iff :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Create an AST node representing t1 implies t2.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac829c0e25bbbd30343bf073f7b524517>
--
foreign import ccall unsafe "Z3_mk_implies"
    z3_mk_implies :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Create an AST node representing t1 xor t2.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gacc6d1b848032dec0c4617b594d4229ec>
--
foreign import ccall unsafe "Z3_mk_xor"
    z3_mk_xor :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Create an AST node representing args[0] and ... and args[num_args-1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gacde98ce4a8ed1dde50b9669db4838c61>
--
foreign import ccall unsafe "Z3_mk_and"
    z3_mk_and :: Ptr Z3_context -> CUInt -> Ptr (Ptr Z3_ast) -> IO (Ptr Z3_ast)

-- | Create an AST node representing args[0] or ... or args[num_args-1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga00866d16331d505620a6c515302021f9>
--
foreign import ccall unsafe "Z3_mk_or"
    z3_mk_or :: Ptr Z3_context -> CUInt -> Ptr (Ptr Z3_ast) -> IO (Ptr Z3_ast)

---------------------------------------------------------------------
-- * Arithmetic: Integers and Reals

-- | Create an AST node representing args[0] + ... + args[num_args-1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4e4ac0a4e53eee0b4b0ef159ed7d0cd5>
--
foreign import ccall unsafe "Z3_mk_add"
    z3_mk_add :: Ptr Z3_context -> CUInt -> Ptr (Ptr Z3_ast) -> IO (Ptr Z3_ast)

-- | Create an AST node representing args[0] * ... * args[num_args-1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab9affbf8401a18eea474b59ad4adc890>
--
foreign import ccall unsafe "Z3_mk_mul"
    z3_mk_mul :: Ptr Z3_context -> CUInt -> Ptr (Ptr Z3_ast) -> IO (Ptr Z3_ast)

-- | Create an AST node representing args[0] - ... - args[num_args - 1].
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4f5fea9b683f9e674fd8f14d676cc9a9>
--
foreign import ccall unsafe "Z3_mk_sub"
    z3_mk_sub :: Ptr Z3_context -> CUInt -> Ptr (Ptr Z3_ast) -> IO (Ptr Z3_ast)

-- | Create an AST node representing -arg.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gadcd2929ad732937e25f34277ce4988ea>
--
foreign import ccall unsafe "Z3_mk_unary_minus"
    z3_mk_unary_minus :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Create an AST node representing arg1 div arg2.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1ac60ee8307af8d0b900375914194ff3>
--
foreign import ccall unsafe "Z3_mk_div"
    z3_mk_div :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast ->  IO (Ptr Z3_ast)

-- | Create an AST node representing arg1 mod arg2.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga8e350ac77e6b8fe805f57efe196e7713>
--
foreign import ccall unsafe "Z3_mk_mod"
    z3_mk_mod :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast ->  IO (Ptr Z3_ast)

-- | Create an AST node representing arg1 rem arg2.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga2fcdb17f9039bbdaddf8a30d037bd9ff>
--
foreign import ccall unsafe "Z3_mk_rem"
    z3_mk_rem :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast ->  IO (Ptr Z3_ast)

-- | Create less than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga58a3dc67c5de52cf599c346803ba1534>
--
foreign import ccall unsafe "Z3_mk_lt"
    z3_mk_lt :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast ->  IO (Ptr Z3_ast)

-- | Create less than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaa9a33d11096841f4e8c407f1578bc0bf>
--
foreign import ccall unsafe "Z3_mk_le"
    z3_mk_le :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast ->  IO (Ptr Z3_ast)

-- | Create greater than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga46167b86067586bb742c0557d7babfd3>
--
foreign import ccall unsafe "Z3_mk_gt"
    z3_mk_gt :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast ->  IO (Ptr Z3_ast)

-- | Create greater than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gad9245cbadb80b192323d01a8360fb942>
--
foreign import ccall unsafe "Z3_mk_ge"
    z3_mk_ge :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast ->  IO (Ptr Z3_ast)

-- | Coerce an integer to a real.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga7130641e614c7ebafd28ae16a7681a21>
--
foreign import ccall unsafe "Z3_mk_int2real"
    z3_mk_int2real :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Coerce a real to an integer.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga759b6563ba1204aae55289009a3fdc6d>
--
foreign import ccall unsafe "Z3_mk_real2int"
    z3_mk_real2int :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Check if a real number is an integer.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaac2ad0fb04e4900fdb4add438d137ad3>
--
foreign import ccall unsafe "Z3_mk_is_int"
    z3_mk_is_int :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_ast)

---------------------------------------------------------------------
-- * Bit-vectors

-- | Bitwise negation.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga36cf75c92c54c1ca633a230344f23080>
--
foreign import ccall unsafe "Z3_mk_bvnot"
    z3_mk_bvnot :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Take conjunction of bits in vector, return vector of length 1.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaccc04f2b58903279b1b3be589b00a7d8>
--
foreign import ccall unsafe "Z3_mk_bvredand"
    z3_mk_bvredand :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Take disjunction of bits in vector, return vector of length 1.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gafd18e127c0586abf47ad9cd96895f7d2>
--
foreign import ccall unsafe "Z3_mk_bvredor"
    z3_mk_bvredor :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Bitwise and.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab96e0ea55334cbcd5a0e79323b57615d>
--
foreign import ccall unsafe "Z3_mk_bvand"
    z3_mk_bvand :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Bitwise or.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga77a6ae233fb3371d187c6d559b2843f5>
--
foreign import ccall unsafe "Z3_mk_bvor"
    z3_mk_bvor :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Bitwise exclusive-or.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0a3821ea00b1c762205f73e4bc29e7d8>
--
foreign import ccall unsafe "Z3_mk_bvxor"
    z3_mk_bvxor :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Bitwise nand.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga96dc37d36efd658fff5b2b4df49b0e61>
--
foreign import ccall unsafe "Z3_mk_bvnand"
    z3_mk_bvnand :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Bitwise nor.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gabf15059e9e8a2eafe4929fdfd259aadb>
--
foreign import ccall unsafe "Z3_mk_bvnor"
    z3_mk_bvnor :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Bitwise xnor.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga784f5ca36a4b03b93c67242cc94b21d6>
--
foreign import ccall unsafe "Z3_mk_bvxnor"
    z3_mk_bvxnor :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Standard two's complement unary minus.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0c78be00c03eda4ed6a983224ed5c7b7
--
foreign import ccall unsafe "Z3_mk_bvneg"
    z3_mk_bvneg :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Standard two's complement addition.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga819814e33573f3f9948b32fdc5311158>
--
foreign import ccall unsafe "Z3_mk_bvadd"
    z3_mk_bvadd :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Standard two's complement subtraction.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga688c9aa1347888c7a51be4e46c19178e>
--
foreign import ccall unsafe "Z3_mk_bvsub"
    z3_mk_bvsub :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Standard two's complement multiplication.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga6abd3dde2a1ceff1704cf7221a72258c>
--
foreign import ccall unsafe "Z3_mk_bvmul"
    z3_mk_bvmul :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Unsigned division.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga56ce0cd61666c6f8cf5777286f590544>
--
foreign import ccall unsafe "Z3_mk_bvudiv"
    z3_mk_bvudiv :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Two's complement signed division.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gad240fedb2fda1c1005b8e9d3c7f3d5a0>
--
foreign import ccall unsafe "Z3_mk_bvsdiv"
    z3_mk_bvsdiv :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Unsigned remainder.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5df4298ec835e43ddc9e3e0bae690c8d>
--
foreign import ccall unsafe "Z3_mk_bvurem"
    z3_mk_bvurem :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Two's complement signed remainder (sign follows dividend).
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga46c18a3042fca174fe659d3185693db1>
--
foreign import ccall unsafe "Z3_mk_bvsrem"
    z3_mk_bvsrem :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Two's complement signed remainder (sign follows divisor).
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga95dac8e6eecb50f63cb82038560e0879>
--
foreign import ccall unsafe "Z3_mk_bvsmod"
    z3_mk_bvsmod :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Unsigned less than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5774b22e93abcaf9b594672af6c7c3c4>
--
foreign import ccall unsafe "Z3_mk_bvult"
    z3_mk_bvult :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Two's complement signed less than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga8ce08af4ed1fbdf08d4d6e63d171663a>
--
foreign import ccall unsafe "Z3_mk_bvslt"
    z3_mk_bvslt :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Unsigned less than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab738b89de0410e70c089d3ac9e696e87>
--
foreign import ccall unsafe "Z3_mk_bvule"
    z3_mk_bvule :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Two's complement signed less than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab7c026feb93e7d2eab180e96f1e6255d>
--
foreign import ccall unsafe "Z3_mk_bvsle"
    z3_mk_bvsle :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Unsigned greater than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gade58fbfcf61b67bf8c4a441490d3c4df>
--
foreign import ccall unsafe "Z3_mk_bvuge"
    z3_mk_bvuge :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Two's complement signed greater than or equal to.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaeec3414c0e8a90a6aa5a23af36bf6dc5>
--
foreign import ccall unsafe "Z3_mk_bvsge"
    z3_mk_bvsge :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Unsigned greater than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga063ab9f16246c99e5c1c893613927ee3>
--
foreign import ccall unsafe "Z3_mk_bvugt"
    z3_mk_bvugt :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Two's complement signed greater than.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4e93a985aa2a7812c7c11a2c65d7c5f0>
--
foreign import ccall unsafe "Z3_mk_bvsgt"
    z3_mk_bvsgt :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Concatenate the given bit-vectors.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae774128fa5e9ff7458a36bd10e6ca0fa>
--
foreign import ccall unsafe "Z3_mk_concat"
    z3_mk_concat :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Extract the bits high down to low from a bitvector of size m to yield a new
-- bitvector of size /n/, where /n = high - low + 1/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga32d2fe7563f3e6b114c1b97b205d4317>
--
foreign import ccall unsafe "Z3_mk_extract"
    z3_mk_extract :: Ptr Z3_context -> CUInt -> CUInt -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Sign-extend of the given bit-vector to the (signed) equivalent bitvector
-- of size /m+i/, where /m/ is the size of the given bit-vector.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gad29099270b36d0680bb54b560353c10e>
--
foreign import ccall unsafe "Z3_mk_sign_ext"
    z3_mk_sign_ext :: Ptr Z3_context -> CUInt -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Extend the given bit-vector with zeros to the (unsigned) equivalent
-- bitvector of size /m+i/, where /m/ is the size of the given bit-vector.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac9322fae11365a78640baf9078c428b3>
--
foreign import ccall unsafe "Z3_mk_zero_ext"
    z3_mk_zero_ext :: Ptr Z3_context -> CUInt -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Repeat the given bit-vector up length /i/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga03e81721502ea225c264d1f556c9119d>
--
foreign import ccall unsafe "Z3_mk_repeat"
    z3_mk_repeat :: Ptr Z3_context -> CUInt -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Shift left.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac8d5e776c786c1172fa0d7dfede454e1>
--
foreign import ccall unsafe "Z3_mk_bvshl"
    z3_mk_bvshl :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Logical shift right.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac59645a6edadad79a201f417e4e0c512>
--
foreign import ccall unsafe "Z3_mk_bvlshr"
    z3_mk_bvlshr :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Arithmetic shift right.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga674b580ad605ba1c2c9f9d3748be87c4>
--
foreign import ccall unsafe "Z3_mk_bvashr"
    z3_mk_bvashr :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Rotate bits of /t1/ to the left /i/ times.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4932b7d08fea079dd903cd857a52dcda>
--
foreign import ccall unsafe "Z3_mk_rotate_left"
    z3_mk_rotate_left :: Ptr Z3_context -> CUInt -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Rotate bits of /t1/ to the right /i/ times.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga3b94e1bf87ecd1a1858af8ebc1da4a1c>
--
foreign import ccall unsafe "Z3_mk_rotate_right"
    z3_mk_rotate_right :: Ptr Z3_context -> CUInt -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Rotate bits of /t1/ to the left /t2/ times.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaf46f1cb80e5a56044591a76e7c89e5e7>
--
foreign import ccall unsafe "Z3_mk_ext_rotate_left"
    z3_mk_ext_rotate_left :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Rotate bits of /t1/ to the right /t2/ times.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gabb227526c592b523879083f12aab281f>
--
foreign import ccall unsafe "Z3_mk_ext_rotate_right"
    z3_mk_ext_rotate_right :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Create an /n/ bit bit-vector from the integer argument /t1/.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga35f89eb05df43fbd9cce7200cc1f30b5>
--
foreign import ccall unsafe "Z3_mk_"
    z3_mk_int2bv :: Ptr Z3_context -> CUInt -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Create an integer from the bit-vector argument /t1/. If /is_signed/ is false,
-- then the bit-vector /t1/ is treated as unsigned. So the result is non-negative
-- and in the range [0..2^/N/-1], where /N/ are the number of bits in /t1/.
-- If /is_signed/ is true, /t1/ is treated as a signed bit-vector.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac87b227dc3821d57258d7f53a28323d4>
--
foreign import ccall unsafe "Z3_mk_bv2int"
    z3_mk_bv2int :: Ptr Z3_context -> Ptr Z3_ast -> Z3_bool -> IO (Ptr Z3_ast)

-- | Create a predicate that checks that the bit-wise addition of /t1/ and /t2/
-- does not overflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga88f6b5ec876f05e0d7ba51e96c4b077f>
--
foreign import ccall unsafe "Z3_mk_bvadd_no_overflow"
    z3_mk_bvadd_no_overflow :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> Z3_bool -> IO (Ptr Z3_ast)

-- | Create a predicate that checks that the bit-wise signed addition of /t1/
-- and /t2/ does not underflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1e2b1927cf4e50000c1600d47a152947>
--
foreign import ccall unsafe "Z3_mk_bvadd_no_underflow"
    z3_mk_bvadd_no_underflow :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Create a predicate that checks that the bit-wise signed subtraction of /t1/
-- and /t2/ does not overflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga785f8127b87e0b42130e6d8f52167d7c>
--
foreign import ccall unsafe "Z3_mk_bvsub_no_overflow"
    z3_mk_bvsub_no_overflow :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Create a predicate that checks that the bit-wise subtraction of /t1/ and
-- /t2/ does not underflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga6480850f9fa01e14aea936c88ff184c4>
--
foreign import ccall unsafe "Z3_mk_bvsub_no_underflow"
    z3_mk_bvsub_no_underflow :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Create a predicate that checks that the bit-wise signed division of /t1/
-- and /t2/ does not overflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaa17e7b2c33dfe2abbd74d390927ae83e>
--
foreign import ccall unsafe "Z3_mk_bvsdiv_no_overflow"
    z3_mk_bvsdiv_no_overflow :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Check that bit-wise negation does not overflow when /t1/ is interpreted as
-- a signed bit-vector.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae9c5d72605ddcd0e76657341eaccb6c7>
--
foreign import ccall unsafe "Z3_mk_bvneg_no_overflow"
    z3_mk_bvneg_no_overflow :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Create a predicate that checks that the bit-wise multiplication of /t1/ and
-- /t2/ does not overflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga86f4415719d295a2f6845c70b3aaa1df>
--
foreign import ccall unsafe "Z3_mk_bvmul_no_overflow"
    z3_mk_bvmul_no_overflow :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> Z3_bool -> IO (Ptr Z3_ast)

-- | Create a predicate that checks that the bit-wise signed multiplication of
-- /t1/ and /t2/ does not underflow.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga501ccc01d737aad3ede5699741717fda>
--
foreign import ccall unsafe "Z3_mk_bvmul_no_underflow"
    z3_mk_bvmul_no_underflow :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- TODO Arrays, Sets

---------------------------------------------------------------------
-- * Numerals

-- | Create a numeral of a given sort.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac8aca397e32ca33618d8024bff32948c>
--
foreign import ccall unsafe "Z3_mk_numeral"
    z3_mk_numeral :: Ptr Z3_context -> Z3_string -> Ptr Z3_sort ->  IO (Ptr Z3_ast)

-- | Create a real from a fraction.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gabe0bbc1e01a084a75506a62e5e6900b3>
--
foreign import ccall unsafe "Z3_mk_real"
    z3_mk_real :: Ptr Z3_context -> CInt -> CInt -> IO (Ptr Z3_ast)

-- | Create a numeral of a given sort.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga8779204998136569c3e166c34cfd3e2c>
--
foreign import ccall unsafe "Z3_mk_int"
    z3_mk_int :: Ptr Z3_context -> CInt -> Ptr Z3_sort ->  IO (Ptr Z3_ast)

-- | Create a numeral of a given sort.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga7201b6231b61421c005457206760a121>
--
foreign import ccall unsafe "Z3_mk_unsigned_int"
    z3_mk_unsigned_int :: Ptr Z3_context -> CUInt -> Ptr Z3_sort ->  IO (Ptr Z3_ast)

-- | Create a numeral of a given sort.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga42cc319787d485d9cb665d80e02d206f>
--
foreign import ccall unsafe "Z3_mk_int64"
    z3_mk_int64 :: Ptr Z3_context -> CLLong -> Ptr Z3_sort ->  IO (Ptr Z3_ast)

-- | Create a numeral of a given sort.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga88a165138162a8bac401672f0a1b7891>
--
foreign import ccall unsafe "Z3_mk_unsigned_int64"
    z3_mk_unsigned_int64 :: Ptr Z3_context -> CULLong -> Ptr Z3_sort ->  IO (Ptr Z3_ast)

---------------------------------------------------------------------
-- * Quantifiers

-- | Create a pattern for quantifier instantiation.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaf15c95b66dc3b0af735774ee401a6f85>
--
foreign import ccall unsafe "Z3_mk_pattern"
  z3_mk_pattern :: Ptr Z3_context -> CUInt -> Ptr (Ptr Z3_ast) -> IO (Ptr Z3_pattern)

-- | Create a bound variable.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1d4da8849fca699b345322f8ee1fa31e>
--
foreign import ccall unsafe "Z3_mk_bound"
  z3_mk_bound :: Ptr Z3_context -> CUInt -> Ptr Z3_sort -> IO (Ptr Z3_ast)

-- | Create a forall formula.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga7e975b7d7ac96de1db73d8f71166c663>
--
foreign import ccall unsafe "Z3_mk_forall"
  z3_mk_forall :: Ptr Z3_context -> CUInt
                  -> CUInt -> Ptr (Ptr Z3_pattern)
                  -> CUInt -> Ptr (Ptr Z3_sort) -> Ptr (Ptr Z3_symbol)
                  -> Ptr Z3_ast
                  -> IO (Ptr Z3_ast)

-- TODO From 'Z3_mk_exists' on.

---------------------------------------------------------------------
-- * Accessors

-- | Return the size of the given bit-vector sort.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga8fc3550edace7bc046e16d1f96ddb419>
--
foreign import ccall unsafe "Z3_get_bv_sort_size"
    z3_get_bv_sort_size :: Ptr Z3_context -> Ptr Z3_sort -> IO CUInt

-- | Return the sort of an AST node.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0a4dac7e9397ff067136354cd33cb933>
--
foreign import ccall unsafe "Z3_get_sort"
    z3_get_sort :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_sort)

-- | Return Z3_L_TRUE if a is true, Z3_L_FALSE if it is false, and Z3_L_UNDEF
-- otherwise.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga133aaa1ec31af9b570ed7627a3c8c5a4>
--
foreign import ccall unsafe "Z3_get_bool_value"
    z3_get_bool_value :: Ptr Z3_context -> Ptr Z3_ast -> IO Z3_lbool

-- | Return numeral value, as a string of a numeric constant term.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga94617ef18fa7157e1a3f85db625d2f4b>
--
foreign import ccall unsafe "Z3_get_numeral_string"
    z3_get_numeral_string :: Ptr Z3_context -> Ptr Z3_ast -> IO Z3_string

-- TODO Modifiers

---------------------------------------------------------------------
-- * Models

-- | Evaluate the AST node t in the given model. Return Z3_TRUE if succeeded,
-- and store the result in v.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga47d3655283564918c85bda0b423b7f67>
--
foreign import ccall unsafe "Z3_eval"
    z3_eval :: Ptr Z3_context
            -> Ptr Z3_model
            -> Ptr Z3_ast
            -> Ptr (Ptr Z3_ast)
            -> IO Z3_bool

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
foreign import ccall unsafe "Z3_assert_cnstr"
    z3_assert_cnstr :: Ptr Z3_context -> Ptr Z3_ast ->  IO ()

-- | Check whether the given logical context is consistent or not.
--
-- Reference : <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaff310fef80ac8a82d0a51417e073ec0a>
--
foreign import ccall unsafe "Z3_check_and_get_model"
    z3_check_and_get_model :: Ptr Z3_context -> Ptr (Ptr Z3_model) -> IO Z3_lbool

-- | Check whether the given logical context is consistent or not.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga72055cfbae81bd174abed32a83e50b03>
--
foreign import ccall unsafe "Z3_check"
    z3_check :: Ptr Z3_context ->  IO Z3_lbool

-- TODO Constraints: Z3_check_assumptions
-- TODO Constraints: Z3_get_implied_equalities

-- | Delete a model object.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0cc98d3ce68047f873e119bccaabdbee>
--
foreign import ccall unsafe "Z3_del_model"
    z3_del_model :: Ptr Z3_context -> Ptr Z3_model -> IO ()


-- TODO From section 'Constraints' on.

{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE ForeignFunctionInterface   #-}

-- |
-- Module    : Z3.Base.C
-- Copyright : (c) Iago Abal, 2011 
--             (c) David Castro, 2011
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>, 
--             David Castro <david.castro.dcp@gmail.com>


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

-- | abstract syntax tree node. That is, the data-structure used in Z3 to
-- represent terms, formulas and types.
data Z3_ast

-- | A kind of AST used to represent types.
data Z3_sort

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

-- | Create an real type.
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga40ef93b9738485caed6dc84631c3c1a0>
--
foreign import ccall unsafe "Z3_mk_real_sort"
    z3_mk_real_sort :: Ptr Z3_context -> IO (Ptr Z3_sort)

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
foreign import ccall unsafe "Z3_mk_const"
    z3_mk_const :: Ptr Z3_context -> Ptr Z3_symbol -> Ptr Z3_sort -> IO (Ptr Z3_ast)

-- TODO Constants and Applications: Z3_mk_label
-- TODO Constants and Applications: Z3_mk_fresh_func_decl
-- TODO Constants and Applications: Z3_mk_fresh_const

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

-- TODO Constants and applications: bitvectors and arrays
-- TODO Sets

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

-- TODO Quantifiers, Accessors, Modifiers, Coercions

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

-- TODO Constraints: Z3_check_and_get_model

-- | Check whether the given logical context is consistent or not. 
--
-- Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga72055cfbae81bd174abed32a83e50b03>
--
foreign import ccall unsafe "Z3_check"
    z3_check :: Ptr Z3_context ->  IO Z3_lbool

-- TODO Constraints: Z3_check_assumptions
-- TODO Constraints: Z3_get_implied_equalities
-- TODO Constraints: Z3_del_model

-- TODO From section 'Constraints' on.

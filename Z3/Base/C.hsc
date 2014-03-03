{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module    : Z3.Base.C
-- Copyright : (c) Iago Abal, 2012-2014
--             (c) David Castro, 2012-2013
-- License   : BSD3
-- Maintainer: Iago Abal <mail@iagoabal.eu>,
--             David Castro <david.castro.dcp@gmail.com>
--
-- Z3 API foreign imports.

{- HACKING

Add here the foreign import to support a new API function:

* Take a look to a few others foreign imports and follow the same coding style.
    * 2-space wide indentation, no tabs.
    * No trailing spaces, please.
    * ...
* Place the foreign import in the right section, according to the Z3's API documentation.
* Include a reference to the function's API documentation.
-}

module Z3.Base.C where

import Foreign
import Foreign.C.Types
import Foreign.C.String

#include <z3.h>


---------------------------------------------------------------------
-- * Types

data Z3_config

data Z3_context

data Z3_symbol

data Z3_ast

data Z3_sort

data Z3_func_decl

data Z3_app

data Z3_pattern

data Z3_model

data Z3_func_interp

data Z3_func_entry

data Z3_solver

data Z3_params

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga6c2de6ea89b244e37c3ffb17a9ea2a89>
type Z3_lbool = CInt

z3_l_true, z3_l_false, z3_l_undef :: Z3_lbool
z3_l_true  = #const Z3_L_TRUE
z3_l_false = #const Z3_L_FALSE
z3_l_undef = #const Z3_L_UNDEF

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga3a65ded0ada3ee285865759a21140eeb>
type Z3_bool = CInt

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga311274c8a65a5d25cf715ebdf0c68747>
type Z3_error_handler = Ptr Z3_context -> Z3_error_code -> IO ()

z3_true, z3_false :: Z3_lbool
-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gad86c8730a2e4e61bac585b240a6288d4>
z3_true  = #const Z3_TRUE
-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1d9cee57472b2c7623642f123b8f1781>
z3_false = #const Z3_FALSE

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga49f047b93b0282e686956678da5b86b1>
type Z3_string = CString

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0112dc1e8e08a19bf7a4299bb09a9727>
type Z3_ast_print_mode = CInt
z3_print_smtlib_full :: Z3_ast_print_mode
z3_print_smtlib_full = #const Z3_PRINT_SMTLIB_FULL
z3_print_low_level :: Z3_ast_print_mode
z3_print_low_level = #const Z3_PRINT_LOW_LEVEL
z3_print_smtlib_compliant :: Z3_ast_print_mode
z3_print_smtlib_compliant = #const Z3_PRINT_SMTLIB_COMPLIANT
z3_print_smtlib2_compliant :: Z3_ast_print_mode
z3_print_smtlib2_compliant = #const Z3_PRINT_SMTLIB2_COMPLIANT

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaa9f9e7b1b5b81381fab96debbaaa638f>
type Z3_error_code = CInt
#{enum Z3_error_code,
  , z3_ok                = Z3_OK
  , z3_sort_error        = Z3_SORT_ERROR
  , z3_iob               = Z3_IOB
  , z3_invalid_arg       = Z3_INVALID_ARG
  , z3_parser_error      = Z3_PARSER_ERROR
  , z3_no_parser         = Z3_NO_PARSER
  , z3_invalid_pattern   = Z3_INVALID_PATTERN
  , z3_memout_fail       = Z3_MEMOUT_FAIL
  , z3_file_access_error = Z3_FILE_ACCESS_ERROR
  , z3_internal_fatal    = Z3_INTERNAL_FATAL
  , z3_invalid_usage     = Z3_INVALID_USAGE
  , z3_dec_ref_error     = Z3_DEC_REF_ERROR
  , z3_exception         = Z3_EXCEPTION
  }

---------------------------------------------------------------------
-- * Create configuration

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga7d6c40d9b79fe8a8851cc8540970787f>
foreign import ccall unsafe "Z3_mk_config"
    z3_mk_config :: IO (Ptr Z3_config)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5e620acf5d55d0271097c9bb97219774>
foreign import ccall unsafe "Z3_del_config"
    z3_del_config :: Ptr Z3_config -> IO ()

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga001ade87a1671fe77d7e53ed0f4f1ec3>
foreign import ccall unsafe "Z3_set_param_value"
    z3_set_param_value :: Ptr Z3_config -> Z3_string -> Z3_string -> IO ()


---------------------------------------------------------------------
-- * Create context

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0bd93cfab4d749dd3e2f2a4416820a46>
foreign import ccall unsafe "Z3_mk_context"
    z3_mk_context :: Ptr Z3_config -> IO (Ptr Z3_context)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga556eae80ed43ab13e1e7dc3b38c35200>
foreign import ccall unsafe "Z3_del_context"
    z3_del_context :: Ptr Z3_context -> IO ()

---------------------------------------------------------------------
-- * Symbols

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga3df806baf6124df3e63a58cf23e12411>
foreign import ccall unsafe "Z3_mk_int_symbol"
    z3_mk_int_symbol :: Ptr Z3_context -> CInt -> IO (Ptr Z3_symbol)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gafebb0d3c212927cf7834c3a20a84ecae>
foreign import ccall unsafe "Z3_mk_string_symbol"
    z3_mk_string_symbol :: Ptr Z3_context -> Z3_string -> IO (Ptr Z3_symbol)

---------------------------------------------------------------------
-- * Sorts

-- TODO Sorts: Z3_is_eq_sort

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga736e88741af1c178cbebf94c49aa42de>
foreign import ccall unsafe "Z3_mk_uninterpreted_sort"
    z3_mk_uninterpreted_sort :: Ptr Z3_context -> Ptr Z3_symbol -> IO (Ptr Z3_sort)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gacdc73510b69a010b71793d429015f342>
foreign import ccall unsafe "Z3_mk_bool_sort"
    z3_mk_bool_sort :: Ptr Z3_context -> IO (Ptr Z3_sort)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga6cd426ab5748653b77d389fd3eac1015>
foreign import ccall unsafe "Z3_mk_int_sort"
    z3_mk_int_sort :: Ptr Z3_context -> IO (Ptr Z3_sort)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga40ef93b9738485caed6dc84631c3c1a0>
foreign import ccall unsafe "Z3_mk_real_sort"
    z3_mk_real_sort :: Ptr Z3_context -> IO (Ptr Z3_sort)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaeed000a1bbb84b6ca6fdaac6cf0c1688>
foreign import ccall unsafe "Z3_mk_bv_sort"
    z3_mk_bv_sort :: Ptr Z3_context -> CUInt -> IO (Ptr Z3_sort)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gafe617994cce1b516f46128e448c84445>
foreign import ccall unsafe "Z3_mk_array_sort"
    z3_mk_array_sort :: Ptr Z3_context -> Ptr Z3_sort -> Ptr Z3_sort -> IO (Ptr Z3_sort)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga7156b9c0a76a28fae46c81f8e3cdf0f1>
foreign import ccall unsafe "Z3_mk_tuple_sort"
    z3_mk_tuple_sort :: Ptr Z3_context
                     -> Ptr Z3_symbol
                     -> CUInt
                     -> Ptr (Ptr Z3_symbol)
                     -> Ptr (Ptr Z3_sort)
                     -> Ptr (Ptr Z3_func_decl)
                     -> Ptr (Ptr Z3_func_decl)
                     -> IO (Ptr Z3_sort)

-- TODO Sorts: from Z3_mk_array_sort on


---------------------------------------------------------------------
-- * Constants and Applications

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaa5c5e2602a44d5f1373f077434859ca2>
foreign import ccall unsafe "Z3_mk_func_decl"
    z3_mk_func_decl :: Ptr Z3_context
                         -> Ptr Z3_symbol
                         -> CUInt
                         -> Ptr (Ptr Z3_sort)
                         -> Ptr Z3_sort
                         -> IO (Ptr Z3_func_decl)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga33a202d86bf628bfab9b6f437536cebe>
foreign import ccall unsafe "Z3_mk_app"
    z3_mk_app :: Ptr Z3_context
                   -> Ptr Z3_func_decl
                   -> CUInt
                   -> Ptr (Ptr Z3_ast)
                   -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga093c9703393f33ae282ec5e8729354ef>
foreign import ccall unsafe "Z3_mk_const"
    z3_mk_const :: Ptr Z3_context -> Ptr Z3_symbol -> Ptr Z3_sort -> IO (Ptr Z3_ast)

-- TODO Constants and Applications: Z3_mk_fresh_func_decl
-- TODO Constants and Applications: Z3_mk_fresh_const

---------------------------------------------------------------------
-- * Propositional Logic and Equality

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae898e7380409bbc57b56cc5205ef1db7>
foreign import ccall unsafe "Z3_mk_true"
    z3_mk_true :: Ptr Z3_context -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5952ac17671117a02001fed6575c778d>
foreign import ccall unsafe "Z3_mk_false"
    z3_mk_false :: Ptr Z3_context -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga95a19ce675b70e22bb0401f7137af37c>
foreign import ccall unsafe "Z3_mk_eq"
    z3_mk_eq :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaa076d3a668e0ec97d61744403153ecf7>
foreign import ccall unsafe "Z3_mk_distinct"
    z3_mk_distinct :: Ptr Z3_context -> CUInt -> Ptr (Ptr Z3_ast) -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga3329538091996eb7b3dc677760a61072>
foreign import ccall unsafe "Z3_mk_not"
    z3_mk_not :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga94417eed5c36e1ad48bcfc8ad6e83547>
foreign import ccall unsafe "Z3_mk_ite"
    z3_mk_ite :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga930a8e844d345fbebc498ac43a696042>
foreign import ccall unsafe "Z3_mk_iff"
    z3_mk_iff :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac829c0e25bbbd30343bf073f7b524517>
foreign import ccall unsafe "Z3_mk_implies"
    z3_mk_implies :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gacc6d1b848032dec0c4617b594d4229ec>
foreign import ccall unsafe "Z3_mk_xor"
    z3_mk_xor :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gacde98ce4a8ed1dde50b9669db4838c61>
foreign import ccall unsafe "Z3_mk_and"
    z3_mk_and :: Ptr Z3_context -> CUInt -> Ptr (Ptr Z3_ast) -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga00866d16331d505620a6c515302021f9>
foreign import ccall unsafe "Z3_mk_or"
    z3_mk_or :: Ptr Z3_context -> CUInt -> Ptr (Ptr Z3_ast) -> IO (Ptr Z3_ast)

---------------------------------------------------------------------
-- * Arithmetic: Integers and Reals

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4e4ac0a4e53eee0b4b0ef159ed7d0cd5>
foreign import ccall unsafe "Z3_mk_add"
    z3_mk_add :: Ptr Z3_context -> CUInt -> Ptr (Ptr Z3_ast) -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab9affbf8401a18eea474b59ad4adc890>
foreign import ccall unsafe "Z3_mk_mul"
    z3_mk_mul :: Ptr Z3_context -> CUInt -> Ptr (Ptr Z3_ast) -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4f5fea9b683f9e674fd8f14d676cc9a9>
foreign import ccall unsafe "Z3_mk_sub"
    z3_mk_sub :: Ptr Z3_context -> CUInt -> Ptr (Ptr Z3_ast) -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gadcd2929ad732937e25f34277ce4988ea>
foreign import ccall unsafe "Z3_mk_unary_minus"
    z3_mk_unary_minus :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1ac60ee8307af8d0b900375914194ff3>
foreign import ccall unsafe "Z3_mk_div"
    z3_mk_div :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast ->  IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga8e350ac77e6b8fe805f57efe196e7713>
foreign import ccall unsafe "Z3_mk_mod"
    z3_mk_mod :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast ->  IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga2fcdb17f9039bbdaddf8a30d037bd9ff>
foreign import ccall unsafe "Z3_mk_rem"
    z3_mk_rem :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast ->  IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga58a3dc67c5de52cf599c346803ba1534>
foreign import ccall unsafe "Z3_mk_lt"
    z3_mk_lt :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast ->  IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaa9a33d11096841f4e8c407f1578bc0bf>
foreign import ccall unsafe "Z3_mk_le"
    z3_mk_le :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast ->  IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga46167b86067586bb742c0557d7babfd3>
foreign import ccall unsafe "Z3_mk_gt"
    z3_mk_gt :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast ->  IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gad9245cbadb80b192323d01a8360fb942>
foreign import ccall unsafe "Z3_mk_ge"
    z3_mk_ge :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast ->  IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga7130641e614c7ebafd28ae16a7681a21>
foreign import ccall unsafe "Z3_mk_int2real"
    z3_mk_int2real :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga759b6563ba1204aae55289009a3fdc6d>
foreign import ccall unsafe "Z3_mk_real2int"
    z3_mk_real2int :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaac2ad0fb04e4900fdb4add438d137ad3>
foreign import ccall unsafe "Z3_mk_is_int"
    z3_mk_is_int :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_ast)

---------------------------------------------------------------------
-- * Bit-vectors

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga36cf75c92c54c1ca633a230344f23080>
foreign import ccall unsafe "Z3_mk_bvnot"
    z3_mk_bvnot :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaccc04f2b58903279b1b3be589b00a7d8>
foreign import ccall unsafe "Z3_mk_bvredand"
    z3_mk_bvredand :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gafd18e127c0586abf47ad9cd96895f7d2>
foreign import ccall unsafe "Z3_mk_bvredor"
    z3_mk_bvredor :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab96e0ea55334cbcd5a0e79323b57615d>
foreign import ccall unsafe "Z3_mk_bvand"
    z3_mk_bvand :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga77a6ae233fb3371d187c6d559b2843f5>
foreign import ccall unsafe "Z3_mk_bvor"
    z3_mk_bvor :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0a3821ea00b1c762205f73e4bc29e7d8>
foreign import ccall unsafe "Z3_mk_bvxor"
    z3_mk_bvxor :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga96dc37d36efd658fff5b2b4df49b0e61>
foreign import ccall unsafe "Z3_mk_bvnand"
    z3_mk_bvnand :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gabf15059e9e8a2eafe4929fdfd259aadb>
foreign import ccall unsafe "Z3_mk_bvnor"
    z3_mk_bvnor :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga784f5ca36a4b03b93c67242cc94b21d6>
foreign import ccall unsafe "Z3_mk_bvxnor"
    z3_mk_bvxnor :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0c78be00c03eda4ed6a983224ed5c7b7
foreign import ccall unsafe "Z3_mk_bvneg"
    z3_mk_bvneg :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga819814e33573f3f9948b32fdc5311158>
foreign import ccall unsafe "Z3_mk_bvadd"
    z3_mk_bvadd :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga688c9aa1347888c7a51be4e46c19178e>
foreign import ccall unsafe "Z3_mk_bvsub"
    z3_mk_bvsub :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga6abd3dde2a1ceff1704cf7221a72258c>
foreign import ccall unsafe "Z3_mk_bvmul"
    z3_mk_bvmul :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga56ce0cd61666c6f8cf5777286f590544>
foreign import ccall unsafe "Z3_mk_bvudiv"
    z3_mk_bvudiv :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gad240fedb2fda1c1005b8e9d3c7f3d5a0>
foreign import ccall unsafe "Z3_mk_bvsdiv"
    z3_mk_bvsdiv :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5df4298ec835e43ddc9e3e0bae690c8d>
foreign import ccall unsafe "Z3_mk_bvurem"
    z3_mk_bvurem :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga46c18a3042fca174fe659d3185693db1>
foreign import ccall unsafe "Z3_mk_bvsrem"
    z3_mk_bvsrem :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga95dac8e6eecb50f63cb82038560e0879>
foreign import ccall unsafe "Z3_mk_bvsmod"
    z3_mk_bvsmod :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5774b22e93abcaf9b594672af6c7c3c4>
foreign import ccall unsafe "Z3_mk_bvult"
    z3_mk_bvult :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga8ce08af4ed1fbdf08d4d6e63d171663a>
foreign import ccall unsafe "Z3_mk_bvslt"
    z3_mk_bvslt :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab738b89de0410e70c089d3ac9e696e87>
foreign import ccall unsafe "Z3_mk_bvule"
    z3_mk_bvule :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab7c026feb93e7d2eab180e96f1e6255d>
foreign import ccall unsafe "Z3_mk_bvsle"
    z3_mk_bvsle :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gade58fbfcf61b67bf8c4a441490d3c4df>
foreign import ccall unsafe "Z3_mk_bvuge"
    z3_mk_bvuge :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaeec3414c0e8a90a6aa5a23af36bf6dc5>
foreign import ccall unsafe "Z3_mk_bvsge"
    z3_mk_bvsge :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga063ab9f16246c99e5c1c893613927ee3>
foreign import ccall unsafe "Z3_mk_bvugt"
    z3_mk_bvugt :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4e93a985aa2a7812c7c11a2c65d7c5f0>
foreign import ccall unsafe "Z3_mk_bvsgt"
    z3_mk_bvsgt :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae774128fa5e9ff7458a36bd10e6ca0fa>
foreign import ccall unsafe "Z3_mk_concat"
    z3_mk_concat :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga32d2fe7563f3e6b114c1b97b205d4317>
foreign import ccall unsafe "Z3_mk_extract"
    z3_mk_extract :: Ptr Z3_context -> CUInt -> CUInt -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gad29099270b36d0680bb54b560353c10e>
foreign import ccall unsafe "Z3_mk_sign_ext"
    z3_mk_sign_ext :: Ptr Z3_context -> CUInt -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac9322fae11365a78640baf9078c428b3>
foreign import ccall unsafe "Z3_mk_zero_ext"
    z3_mk_zero_ext :: Ptr Z3_context -> CUInt -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga03e81721502ea225c264d1f556c9119d>
foreign import ccall unsafe "Z3_mk_repeat"
    z3_mk_repeat :: Ptr Z3_context -> CUInt -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac8d5e776c786c1172fa0d7dfede454e1>
foreign import ccall unsafe "Z3_mk_bvshl"
    z3_mk_bvshl :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac59645a6edadad79a201f417e4e0c512>
foreign import ccall unsafe "Z3_mk_bvlshr"
    z3_mk_bvlshr :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga674b580ad605ba1c2c9f9d3748be87c4>
foreign import ccall unsafe "Z3_mk_bvashr"
    z3_mk_bvashr :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4932b7d08fea079dd903cd857a52dcda>
foreign import ccall unsafe "Z3_mk_rotate_left"
    z3_mk_rotate_left :: Ptr Z3_context -> CUInt -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga3b94e1bf87ecd1a1858af8ebc1da4a1c>
foreign import ccall unsafe "Z3_mk_rotate_right"
    z3_mk_rotate_right :: Ptr Z3_context -> CUInt -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaf46f1cb80e5a56044591a76e7c89e5e7>
foreign import ccall unsafe "Z3_mk_ext_rotate_left"
    z3_mk_ext_rotate_left :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gabb227526c592b523879083f12aab281f>
foreign import ccall unsafe "Z3_mk_ext_rotate_right"
    z3_mk_ext_rotate_right :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga35f89eb05df43fbd9cce7200cc1f30b5>
foreign import ccall unsafe "Z3_mk_int2bv"
    z3_mk_int2bv :: Ptr Z3_context -> CUInt -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac87b227dc3821d57258d7f53a28323d4>
foreign import ccall unsafe "Z3_mk_bv2int"
    z3_mk_bv2int :: Ptr Z3_context -> Ptr Z3_ast -> Z3_bool -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga88f6b5ec876f05e0d7ba51e96c4b077f>
foreign import ccall unsafe "Z3_mk_bvadd_no_overflow"
    z3_mk_bvadd_no_overflow :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> Z3_bool -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1e2b1927cf4e50000c1600d47a152947>
foreign import ccall unsafe "Z3_mk_bvadd_no_underflow"
    z3_mk_bvadd_no_underflow :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga785f8127b87e0b42130e6d8f52167d7c>
foreign import ccall unsafe "Z3_mk_bvsub_no_overflow"
    z3_mk_bvsub_no_overflow :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga6480850f9fa01e14aea936c88ff184c4>
foreign import ccall unsafe "Z3_mk_bvsub_no_underflow"
    z3_mk_bvsub_no_underflow :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaa17e7b2c33dfe2abbd74d390927ae83e>
foreign import ccall unsafe "Z3_mk_bvsdiv_no_overflow"
    z3_mk_bvsdiv_no_overflow :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae9c5d72605ddcd0e76657341eaccb6c7>
foreign import ccall unsafe "Z3_mk_bvneg_no_overflow"
    z3_mk_bvneg_no_overflow :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga86f4415719d295a2f6845c70b3aaa1df>
foreign import ccall unsafe "Z3_mk_bvmul_no_overflow"
    z3_mk_bvmul_no_overflow :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> Z3_bool -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga501ccc01d737aad3ede5699741717fda>
foreign import ccall unsafe "Z3_mk_bvmul_no_underflow"
    z3_mk_bvmul_no_underflow :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

--------------------------------------------------------------------------------
-- * Arrays

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga38f423f3683379e7f597a7fe59eccb67>
foreign import ccall unsafe "Z3_mk_select"
    z3_mk_select :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae305a4f54b4a64f7e5973ae6ccb13593>
foreign import ccall unsafe "Z3_mk_store"
    z3_mk_store :: Ptr Z3_context -> Ptr Z3_ast -> Ptr Z3_ast -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga84ea6f0c32b99c70033feaa8f00e8f2d>
foreign import ccall unsafe "Z3_mk_const_array"
    z3_mk_const_array :: Ptr Z3_context -> Ptr Z3_sort -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga9150242d9430a8c3d55d2ca3b9a4362d>
foreign import ccall unsafe "Z3_mk_map"
    z3_mk_map :: Ptr Z3_context -> Ptr Z3_func_decl -> CUInt -> Ptr (Ptr Z3_ast) -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga78e89cca82f0ab4d5f4e662e5e5fba7d>
foreign import ccall unsafe "Z3_mk_array_default"
    z3_mk_array_default :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_ast)

-- TODO Sets

---------------------------------------------------------------------
-- * Numerals

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac8aca397e32ca33618d8024bff32948c>
foreign import ccall unsafe "Z3_mk_numeral"
    z3_mk_numeral :: Ptr Z3_context -> Z3_string -> Ptr Z3_sort ->  IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gabe0bbc1e01a084a75506a62e5e6900b3>
foreign import ccall unsafe "Z3_mk_real"
    z3_mk_real :: Ptr Z3_context -> CInt -> CInt -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga8779204998136569c3e166c34cfd3e2c>
foreign import ccall unsafe "Z3_mk_int"
    z3_mk_int :: Ptr Z3_context -> CInt -> Ptr Z3_sort ->  IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga7201b6231b61421c005457206760a121>
foreign import ccall unsafe "Z3_mk_unsigned_int"
    z3_mk_unsigned_int :: Ptr Z3_context -> CUInt -> Ptr Z3_sort ->  IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga42cc319787d485d9cb665d80e02d206f>
foreign import ccall unsafe "Z3_mk_int64"
    z3_mk_int64 :: Ptr Z3_context -> CLLong -> Ptr Z3_sort ->  IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga88a165138162a8bac401672f0a1b7891>
foreign import ccall unsafe "Z3_mk_unsigned_int64"
    z3_mk_unsigned_int64 :: Ptr Z3_context -> CULLong -> Ptr Z3_sort ->  IO (Ptr Z3_ast)

---------------------------------------------------------------------
-- * Quantifiers

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaf15c95b66dc3b0af735774ee401a6f85>
foreign import ccall unsafe "Z3_mk_pattern"
  z3_mk_pattern :: Ptr Z3_context -> CUInt -> Ptr (Ptr Z3_ast) -> IO (Ptr Z3_pattern)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1d4da8849fca699b345322f8ee1fa31e>
foreign import ccall unsafe "Z3_mk_bound"
  z3_mk_bound :: Ptr Z3_context -> CUInt -> Ptr Z3_sort -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga7e975b7d7ac96de1db73d8f71166c663>
foreign import ccall unsafe "Z3_mk_forall"
  z3_mk_forall :: Ptr Z3_context -> CUInt
                  -> CUInt -> Ptr (Ptr Z3_pattern)
                  -> CUInt -> Ptr (Ptr Z3_sort) -> Ptr (Ptr Z3_symbol)
                  -> Ptr Z3_ast
                  -> IO (Ptr Z3_ast)

-- | Reference <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gabdb40b3ac220bce5a3801e6d29fb3bb6>
foreign import ccall unsafe "Z3_mk_forall_const"
  z3_mk_forall_const :: Ptr Z3_context
                     -> CUInt
                     -> CUInt
                     -> Ptr (Ptr Z3_app)
                     -> CUInt
                     -> Ptr (Ptr Z3_pattern)
                     -> Ptr Z3_ast
                     -> IO (Ptr Z3_ast)

-- | Referece: http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4ffce34ff9117e6243283f11d87c1407
foreign import ccall unsafe "Z3_mk_exists"
  z3_mk_exists :: Ptr Z3_context -> CUInt
                  -> CUInt -> Ptr (Ptr Z3_pattern)
                  -> CUInt -> Ptr (Ptr Z3_sort) -> Ptr (Ptr Z3_symbol)
                  -> Ptr Z3_ast
                  -> IO (Ptr Z3_ast)

-- TODO: Z3_mk_quantifier, Z3_mk_quantifier_ex, Z3_mk_forall_const,
-- Z3_mk_exists_const, Z3_mk_quantifier_const, Z3_mk_quantifier_const_ex

---------------------------------------------------------------------
-- * Accessors

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga8fc3550edace7bc046e16d1f96ddb419>
foreign import ccall unsafe "Z3_get_bv_sort_size"
    z3_get_bv_sort_size :: Ptr Z3_context -> Ptr Z3_sort -> IO CUInt

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0a4dac7e9397ff067136354cd33cb933>
foreign import ccall unsafe "Z3_get_sort"
    z3_get_sort :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_sort)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga133aaa1ec31af9b570ed7627a3c8c5a4>
foreign import ccall unsafe "Z3_get_bool_value"
    z3_get_bool_value :: Ptr Z3_context -> Ptr Z3_ast -> IO Z3_lbool

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga94617ef18fa7157e1a3f85db625d2f4b>
foreign import ccall unsafe "Z3_get_numeral_string"
    z3_get_numeral_string :: Ptr Z3_context -> Ptr Z3_ast -> IO Z3_string

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaf9345fd0822d7e9928dd4ab14a09765b>
foreign import ccall unsafe "Z3_to_app"
  z3_to_app :: Ptr Z3_context -> Ptr Z3_ast -> IO (Ptr Z3_app)

-- TODO Modifiers

---------------------------------------------------------------------
-- * Models

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga47d3655283564918c85bda0b423b7f67>
foreign import ccall unsafe "Z3_eval"
    z3_eval :: Ptr Z3_context
            -> Ptr Z3_model
            -> Ptr Z3_ast
            -> Ptr (Ptr Z3_ast)
            -> IO Z3_bool

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4674da67d226bfb16861829b9f129cfa>
foreign import ccall unsafe "Z3_is_as_array"
    z3_is_as_array :: Ptr Z3_context
                   -> Ptr Z3_ast
                   -> IO Z3_bool

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga7d9262dc6e79f2aeb23fd4a383589dda>
foreign import ccall unsafe "Z3_get_as_array_func_decl"
    z3_get_as_array_func_decl :: Ptr Z3_context
                              -> Ptr Z3_ast
                              -> IO (Ptr Z3_func_decl)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gafb9cc5eca9564d8a849c154c5a4a8633>
foreign import ccall unsafe "Z3_model_get_func_interp"
    z3_model_get_func_interp :: Ptr Z3_context
                             -> Ptr Z3_model
                             -> Ptr Z3_func_decl
                             -> IO (Ptr Z3_func_interp)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga2bab9ae1444940e7593729beec279844>
foreign import ccall unsafe "Z3_func_interp_get_num_entries"
    z3_func_interp_get_num_entries :: Ptr Z3_context
                                   -> Ptr Z3_func_interp
                                   -> IO CUInt

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaf157e1e1cd8c0cfe6a21be6370f659da>
foreign import ccall unsafe "Z3_func_interp_get_entry"
    z3_func_interp_get_entry :: Ptr Z3_context
                             -> Ptr Z3_func_interp
                             -> CUInt
                             -> IO (Ptr Z3_func_entry)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga46de7559826ba71b8488d727cba1fb64>
foreign import ccall unsafe "Z3_func_interp_get_else"
    z3_func_interp_get_else :: Ptr Z3_context
                            -> Ptr Z3_func_interp
                            -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaca22cbdb6f7787aaae5d814f2ab383d8>
foreign import ccall unsafe "Z3_func_interp_get_arity"
    z3_func_interp_get_arity :: Ptr Z3_context
                             -> Ptr Z3_func_interp
                             -> IO CUInt

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga9fd65e2ab039aa8e40608c2ecf7084da>
foreign import ccall unsafe "Z3_func_entry_get_value"
    z3_func_entry_get_value :: Ptr Z3_context
                            -> Ptr Z3_func_entry
                            -> IO (Ptr Z3_ast)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga51aed8c5bc4b1f53f0c371312de3ce1a>
foreign import ccall unsafe "Z3_func_entry_get_num_args"
    z3_func_entry_get_num_args :: Ptr Z3_context
                               -> Ptr Z3_func_entry
                               -> IO CUInt

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga6fe03fe3c824fceb52766a4d8c2cbeab>
foreign import ccall unsafe "Z3_func_entry_get_arg"
    z3_func_entry_get_arg :: Ptr Z3_context
                          -> Ptr Z3_func_entry
                          -> CUInt
                          -> IO (Ptr Z3_ast)

---------------------------------------------------------------------
-- * Constraints

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gad651ad68c7a060cbb5616349233cb10f>
foreign import ccall unsafe "Z3_push"
    z3_push :: Ptr Z3_context -> IO ()

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab2b3a542006c86c8d86dc37872f88b61>
foreign import ccall unsafe "Z3_pop"
    z3_pop :: Ptr Z3_context -> CUInt -> IO ()

-- TODO Constraints: Z3_get_num_scopes
-- TODO Constraints: Z3_persist_ast

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga1a05ff73a564ae7256a2257048a4680a>
foreign import ccall unsafe "Z3_assert_cnstr"
    z3_assert_cnstr :: Ptr Z3_context -> Ptr Z3_ast ->  IO ()

-- | Reference : <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaff310fef80ac8a82d0a51417e073ec0a>
foreign import ccall unsafe "Z3_check_and_get_model"
    z3_check_and_get_model :: Ptr Z3_context -> Ptr (Ptr Z3_model) -> IO Z3_lbool

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga72055cfbae81bd174abed32a83e50b03>
foreign import ccall unsafe "Z3_check"
    z3_check :: Ptr Z3_context ->  IO Z3_lbool

-- TODO Constraints: Z3_check_assumptions
-- TODO Constraints: Z3_get_implied_equalities

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga0cc98d3ce68047f873e119bccaabdbee>
foreign import ccall unsafe "Z3_del_model"
    z3_del_model :: Ptr Z3_context -> Ptr Z3_model -> IO ()

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaf36d49862a8c0d20dd5e6508eef5f8af>
foreign import ccall unsafe "Z3_model_to_string"
    z3_model_to_string :: Ptr Z3_context -> Ptr Z3_model -> IO CString

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga165e38ddfc928f586cb738cdf6c5f216>
foreign import ccall unsafe "Z3_context_to_string"
    z3_context_to_string :: Ptr Z3_context -> IO CString

-- TODO From section 'Constraints' on.


---------------------------------------------------------------------
-- * Parameters

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac7f883536538ab0ad234fde58988e673>
foreign import ccall unsafe "Z3_mk_params"
    z3_mk_params :: Ptr Z3_context -> IO (Ptr Z3_params)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga3a91c9f749b89e1dcf1493177d395d0c>
foreign import ccall unsafe "Z3_params_inc_ref"
    z3_params_inc_ref :: Ptr Z3_context -> Ptr Z3_params -> IO ()

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae4df28ba713b81ee99abd929e32484ea>
foreign import ccall unsafe "Z3_params_dec_ref"
    z3_params_dec_ref :: Ptr Z3_context -> Ptr Z3_params -> IO ()

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga39e3df967eaad45b343256d56c54e91c>
foreign import ccall unsafe "Z3_params_set_bool"
    z3_params_set_bool :: Ptr Z3_context -> Ptr Z3_params -> Ptr Z3_symbol ->
                          Z3_bool -> IO ()

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4974397cb652c7f7f479012eb465e250>
foreign import ccall unsafe "Z3_params_set_uint"
    z3_params_set_uint :: Ptr Z3_context -> Ptr Z3_params -> Ptr Z3_symbol ->
                          CUInt -> IO ()

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga11498ce4b25d294f5f89ab7ac1b74c62>
foreign import ccall unsafe "Z3_params_set_double"
    z3_params_set_double :: Ptr Z3_context -> Ptr Z3_params -> Ptr Z3_symbol ->
                            CDouble -> IO ()

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gac2e899a4906b6133a23fdb60ef992ec9>
foreign import ccall unsafe "Z3_params_set_symbol"
    z3_params_set_symbol :: Ptr Z3_context -> Ptr Z3_params -> Ptr Z3_symbol ->
                            Ptr Z3_symbol -> IO ()

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga624e692e180a8b2f617156b1e1ae9722>
foreign import ccall unsafe "Z3_params_to_string"
    z3_params_to_string :: Ptr Z3_context -> Ptr Z3_params -> IO Z3_string

---------------------------------------------------------------------
-- * Solvers

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5735499ef0b46846c5d45982eaa0e74c>
foreign import ccall unsafe "Z3_mk_solver"
    z3_mk_solver :: Ptr Z3_context -> IO (Ptr Z3_solver)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga5735499ef0b46846c5d45982eaa0e74c>
foreign import ccall unsafe "Z3_mk_simple_solver"
    z3_mk_simple_solver :: Ptr Z3_context -> IO (Ptr Z3_solver)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga54244cfc9d9cd2ca8f08c3909d700628>
foreign import ccall unsafe "Z3_mk_solver_for_logic"
    z3_mk_solver_for_logic :: Ptr Z3_context -> Ptr Z3_symbol -> IO (Ptr Z3_solver)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga887441b3468a1bc605bbf564ddebf2ae>
foreign import ccall unsafe "Z3_solver_set_params"
    z3_solver_set_params :: Ptr Z3_context -> Ptr Z3_solver -> Ptr Z3_params ->
                            IO ()

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga388e25a8b477abbd49f08c6c29dfa12d>
foreign import ccall unsafe "Z3_solver_inc_ref"
    z3_solver_inc_ref :: Ptr Z3_context -> Ptr Z3_solver -> IO ()

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga2362dcef4e9b8ede41298a50428902ff>
foreign import ccall unsafe "Z3_solver_dec_ref"
    z3_solver_dec_ref :: Ptr Z3_context -> Ptr Z3_solver -> IO ()

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae41bebe15b1b1105f9abb8690188d1e2>
foreign import ccall unsafe "Z3_solver_push"
    z3_solver_push :: Ptr Z3_context -> Ptr Z3_solver -> IO ()

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga40aa98e15aceffa5be3afad2e065478a>
foreign import ccall unsafe "Z3_solver_pop"
    z3_solver_pop :: Ptr Z3_context -> Ptr Z3_solver -> CUInt -> IO ()

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gafd4b4a6465601835341b477b75725b28>
foreign import ccall unsafe "Z3_solver_get_num_scopes"
    z3_solver_get_num_scopes :: Ptr Z3_context -> Ptr Z3_solver -> IO CUInt

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga4a4a215b9130d7980e3c393fe857335f>
foreign import ccall unsafe "Z3_solver_reset"
    z3_solver_reset :: Ptr Z3_context -> Ptr Z3_solver -> IO ()

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga72afadf5e8b216f2c6ae675e872b8be4>
foreign import ccall unsafe "Z3_solver_assert"
    z3_solver_assert :: Ptr Z3_context -> Ptr Z3_solver -> Ptr Z3_ast -> IO ()

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaf46fb6f3aa3ef451d6be01a737697810>
foreign import ccall unsafe "Z3_solver_assert_and_track"
    z3_solver_assert_and_track :: Ptr Z3_context -> Ptr Z3_solver ->
                                  Ptr Z3_ast -> Ptr Z3_ast -> IO ()

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga000e369de7b71caa4ee701089709c526>
foreign import ccall unsafe "Z3_solver_check"
    z3_solver_check :: Ptr Z3_context -> Ptr Z3_solver -> IO Z3_lbool

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaf14a54d904a7e45eecc00c5fb8a9d5c9>
foreign import ccall unsafe "Z3_solver_get_model"
    z3_solver_get_model :: Ptr Z3_context -> Ptr Z3_solver -> IO (Ptr Z3_model)

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaed5d19000004b43dd75e487682e91b55>
foreign import ccall unsafe "Z3_solver_get_reason_unknown"
    z3_solver_get_reason_unknown :: Ptr Z3_context -> Ptr Z3_solver ->
                                    IO Z3_string

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaf52e41db4b12a84188b80255454d3abb>
foreign import ccall unsafe "Z3_solver_to_string"
    z3_solver_to_string :: Ptr Z3_context -> Ptr Z3_solver -> IO Z3_string

---------------------------------------------------------------------
-- * String Conversion

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga20d66dac19b6d6a06537843d0e25f761>
foreign import ccall unsafe "Z3_set_ast_print_mode"
    z3_set_ast_print_mode :: Ptr Z3_context -> Z3_ast_print_mode -> IO ()

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gab1aa4b78298fe00b3167bf7bfd88aea3>
foreign import ccall unsafe "Z3_ast_to_string"
    z3_ast_to_string :: Ptr Z3_context -> Ptr Z3_ast -> IO Z3_string

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga51b048ddbbcd88708e7aa4fe1c2462d6>
foreign import ccall unsafe "Z3_pattern_to_string"
    z3_pattern_to_string :: Ptr Z3_context -> Ptr Z3_pattern -> IO Z3_string

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaf90c72f63eab298e1dd750f6a26fb945>
foreign import ccall unsafe "Z3_sort_to_string"
    z3_sort_to_string :: Ptr Z3_context -> Ptr Z3_sort -> IO Z3_string

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga15243dcad77f5571e28e8aa1da465675>
foreign import ccall unsafe "Z3_func_decl_to_string"
    z3_func_decl_to_string :: Ptr Z3_context -> Ptr Z3_func_decl -> IO Z3_string

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaf93844a5964ad8dee609fac3470d86e4>
foreign import ccall unsafe "Z3_benchmark_to_smtlib_string"
    z3_benchmark_to_smtlib_string :: Ptr Z3_context
                                      -> Z3_string        -- ^ name
                                      -> Z3_string        -- ^ logic
                                      -> Z3_string        -- ^ status
                                      -> Z3_string        -- ^ attributes
                                      -> CUInt            -- ^ assumptions#
                                      -> Ptr (Ptr Z3_ast) -- ^ assumptions
                                      -> Ptr Z3_ast       -- ^ formula
                                      -> IO Z3_string

---------------------------------------------------------------------
-- * Error Handling

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga8ac771e68b28d2c86f40aa84889b3807>
foreign import ccall unsafe "Z3_get_error_code"
    z3_get_error_code :: Ptr Z3_context -> IO Z3_error_code

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gadaa12e9990f37b0c1e2bf1dd502dbf39>
foreign import ccall unsafe "Z3_set_error_handler"
    z3_set_error_handler :: Ptr Z3_context -> FunPtr Z3_error_handler -> IO ()

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga41cf70319c4802ab7301dd168d6f5e45>
foreign import ccall unsafe "Z3_set_error"
    z3_set_error :: Ptr Z3_context -> Z3_error_code -> IO ()

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gaf06357c49299efb8a0bdaeb3bc96c6d6>
foreign import ccall unsafe "Z3_get_error_msg"
    z3_get_error_msg :: Z3_error_code -> IO Z3_string

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#gae0aba52b5738b2ea78e0d6ad67ef1f92>
foreign import ccall unsafe "Z3_get_error_msg_ex"
    z3_get_error_msg_ex :: Ptr Z3_context -> Z3_error_code -> IO Z3_string

---------------------------------------------------------------------
-- * Miscellaneous

-- | Reference: <http://research.microsoft.com/en-us/um/redmond/projects/z3/group__capi.html#ga45fcd18a00379b13a536c5b6117190ae>
foreign import ccall unsafe "Z3_get_version"
    z3_get_version :: Ptr CUInt -> Ptr CUInt -> Ptr CUInt -> Ptr CUInt -> IO ()

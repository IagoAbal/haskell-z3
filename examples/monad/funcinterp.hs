module Main where

import Control.Applicative
import Control.Monad ( join )
import Control.Monad.Trans

import Data.Maybe

import Z3.Monad


script :: Z3 [([Integer], Integer)] -- Map (Int, Int) Int)
script =
    do intSort <- mkIntSort
       fSym    <- mkStringSymbol "f"
       fDecl   <- mkFuncDecl fSym [intSort, intSort] intSort

       i1      <- mkInt 5
       i2      <- mkInt 10
       i3      <- mkInt 42

       v       <- mkApp fDecl [i1, i2]

       mkGt v i3 >>= assertCnstr
       (_res, modelMb) <- getModel
       case modelMb of
         Just model -> 
             do Just fs <- evalFunc model fDecl
                let toIntsPair (is, i) =
                        do is' <- mapM getInt is
                           i' <- getInt i
                           return (is', i')
                mapM toIntsPair fs
         Nothing -> error "Couldn't construct model"

main = evalZ3 script >>= print

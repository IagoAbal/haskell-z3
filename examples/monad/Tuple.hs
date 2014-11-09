module Main where

import Z3.Monad

tupleScript :: Z3 String
tupleScript =
    do intSort <- mkIntSort
       argSym  <- mkStringSymbol "arg"
       arg1Sym <- mkStringSymbol "arg1"
       arg2Sym <- mkStringSymbol "arg2"
       (argSort, constr, [proj1, proj2]) <-
           mkTupleSort argSym [(arg1Sym, intSort), (arg2Sym, intSort)]

       tSym <- mkStringSymbol "t"
       t <- mkConst tSym argSort

       p2 <- mkApp proj2 [t]
       mkInt (5 :: Int) >>= mkGt p2 >>= assertCnstr

       (_res, modelMb) <- getModel
       case modelMb of
         Just model -> showModel model
         Nothing -> return "Couldn't construct model"


main :: IO ()
main = evalZ3 tupleScript >>= putStrLn

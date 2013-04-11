module Main where

import Z3.Monad

type RetType = [([Integer], Integer)]

toIntsPair :: ([AST], AST) -> Z3 ([Integer], Integer)
toIntsPair (is, i) =
    do is' <- mapM getInt is
       i' <- getInt i
       return (is', i')

arrayScript :: Z3 (RetType, RetType)  -- Map (Int, Int) Int)
arrayScript =
    do intSort <- mkIntSort
       arrSort <- mkArraySort intSort intSort

       a1Sym   <- mkStringSymbol "a1"
       a2Sym   <- mkStringSymbol "a2"

       a1      <- mkConst a1Sym arrSort
       a2      <- mkConst a2Sym arrSort

       
       i1      <- mkInt (5 :: Int)
       i2      <- mkInt (10 :: Int)
       a1Val1  <- mkSelect a1 i1
       mkGt a1Val1 i2 >>= assertCnstr

       i3      <- mkInt (42 :: Int)
       i4      <- mkInt (81 :: Int)
       a3      <- mkStore a1 i3 i4
       mkEq a2 a3 >>= assertCnstr

       let 
           convertArr :: Model -> AST -> Z3 RetType
           convertArr model arr = 
               do Just fs <- evalArray model arr
                  mapM toIntsPair fs

       (_res, modelMb) <- getModel
       case modelMb of
         Just model -> 
             do a1' <- convertArr model a1
                a2' <- convertArr model a2
                return (a1', a2')
         Nothing -> error "Couldn't construct model"



funcScript :: Z3 RetType
funcScript =
    do intSort <- mkIntSort
       fSym    <- mkStringSymbol "f"
       fDecl   <- mkFuncDecl fSym [intSort, intSort] intSort

       i1      <- mkInt (5 :: Int)
       i2      <- mkInt (10 :: Int)
       i3      <- mkInt (42 :: Int)

       v       <- mkApp fDecl [i1, i2]

       mkGt v i3 >>= assertCnstr
       (_res, modelMb) <- getModel
       case modelMb of
         Just model -> 
             do Just fs <- evalFunc model fDecl
                mapM toIntsPair fs
         Nothing -> error "Couldn't construct model"

main :: IO ()
main = 
    do evalZ3 arrayScript >>= print
       evalZ3 funcScript >>= print

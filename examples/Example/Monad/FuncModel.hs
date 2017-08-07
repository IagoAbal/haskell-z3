-- | Extracting function and array interpretations.
module Example.Monad.FuncModel
  ( run )
  where

import Z3.Monad

run :: IO ()
run =
    do evalZ3 arrayScript >>= print
       evalZ3 funcScript >>= print

type RetType = ([([Integer], Integer)], Integer)

toIntsPair :: ([AST], AST) -> Z3 ([Integer], Integer)
toIntsPair (is, i) =
    do is' <- mapM getInt is
       i' <- getInt i
       return (is', i')

toRetType :: FuncModel -> Z3 RetType
toRetType (FuncModel fs elsePart) =
    do fs' <- mapM toIntsPair fs
       elsePart' <- getInt elsePart
       return (fs', elsePart')

-- * Arrays

-- FIXME: This script crashes when calling 'convertArr', which mysteriously now
-- returns 'Nothing'. It seems that 'isAsArray' is returning 'False' for 'a1'.
arrayScript :: Z3 (RetType, RetType)
arrayScript =
    do intSort <- mkIntSort
       arrSort <- mkArraySort intSort intSort

       a1Sym   <- mkStringSymbol "a1"
       a2Sym   <- mkStringSymbol "a2"

       a1      <- mkConst a1Sym arrSort
       a2      <- mkConst a2Sym arrSort


       i1      <- mkIntNum (5 :: Int)
       i2      <- mkIntNum (10 :: Int)
       a1Val1  <- mkSelect a1 i1
       mkGt a1Val1 i2 >>= assert

       i3      <- mkIntNum (42 :: Int)
       i4      <- mkIntNum (81 :: Int)
       a3      <- mkStore a1 i3 i4
       mkEq a2 a3 >>= assert

       let
           convertArr :: Model -> AST -> Z3 RetType
           convertArr model arr =
               do Just f <- evalArray model arr
                  toRetType f

       (_res, modelMb) <- getModel
       case modelMb of
         Just model ->
             do a1' <- convertArr model a1
                a2' <- convertArr model a2
                return (a1', a2')
         Nothing -> error "Couldn't construct model"

-- * Functions

funcScript :: Z3 RetType
funcScript = do
  -- f :: (Integer,Integer) -> Integer
  intSort <- mkIntSort
  fDecl   <- mkFreshFuncDecl "f" [intSort, intSort] intSort

  -- f(5,10) > 42
  i5      <- mkIntNum (5 :: Integer)
  i10     <- mkIntNum (10 :: Integer)
  i42     <- mkIntNum (42 :: Integer)
  r       <- mkApp fDecl [i5, i10]
  assert =<< mkGt r i42

  -- check satisfiability and obtain model
  (res, mbModel) <- getModel
  case mbModel of
       Just model -> do
         Just f <- evalFunc model fDecl
         toRetType f
       Nothing -> error ("Couldn't construct model: " ++ show res)


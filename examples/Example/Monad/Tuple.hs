-- | Create and use tuples.
module Example.Monad.Tuple
  ( run )
  where

import Z3.Monad

run :: IO ()
run = evalZ3 script >>= putStrLn

script :: Z3 String
script = do
  -- newtype Tup = Tup { arg1 :: Int, arg2 :: Int }
  intSort <- mkIntSort
  tupSym  <- mkStringSymbol "Tup"
  arg1Sym <- mkStringSymbol "arg1"
  arg2Sym <- mkStringSymbol "arg2"
  (tupSort, _constr, [_proj1, proj2]) <-
      mkTupleSort tupSym [(arg1Sym, intSort), (arg2Sym, intSort)]
  -- t :: Tup
  tSym <- mkStringSymbol "t"
  t <- mkConst tSym tupSort
  -- arg2 t > 5
  p2 <- mkApp proj2 [t]
  assert =<< mkGt p2 =<< mkIntNum (5 :: Integer)
  -- get interpretation for t
  (_res, mbModel) <- getModel
  case mbModel of
       Just model -> showModel model
       Nothing    -> return "Couldn't construct model"

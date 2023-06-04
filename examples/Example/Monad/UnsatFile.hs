module Example.Monad.UnsatFile where

import Z3.Monad

run :: IO ()
run =
  evalZ3 result >>= print

result :: MonadZ3 z3 => z3 Result
result = do
  fil <- parseSMTLib2File "examples/SMTLibFiles/Unsat.smtlib" [] [] [] []
  solverCheckAssumptions fil

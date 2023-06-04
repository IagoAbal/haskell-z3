module Example.Monad.SatFile where

import Z3.Monad

run :: IO ()
run =
  evalZ3 result >>= print

result :: MonadZ3 z3 => z3 (Result, Maybe String)
result = do
  fil <- parseSMTLib2File "examples/SMTLibFiles/Sat.smtlib" [] [] [] []
  mapM_ assert fil
  (res, mModel) <- solverCheckAndGetModel
  strModel <- traverse showModel mModel
  return (res, strModel)

-- | Parse AST from SMTLIB string
module Example.Monad.ParserInterface
  ( run )
  where

import Z3.Monad

run :: IO ()
run = evalZ3 script >>= print

-- Toy example SMTLIB string
smtStr1 :: String
smtStr1 = "(declare-const x Int)\n(assert (< x 5))"

smtStr2 :: String
smtStr2 = "(declare-const x Int)\n(assert (> x 5))"


script :: Z3 Result
script = do
  l <- parseSMTLib2String smtStr1 [] [] [] []
  r <- parseSMTLib2String smtStr2 [] [] [] []
  eq <- mkEq l r
  assert l
  assert r
  assert eq
  check

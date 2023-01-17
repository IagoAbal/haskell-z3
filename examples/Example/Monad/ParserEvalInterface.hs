-- | Parse SMTLIB string and evaluate it, line by line
module Example.Monad.ParserEvalInterface
  ( run )
  where

import Z3.Monad

run :: IO ()
run = do
  outputs <- evalZ3 script
  traverse putStr outputs >> return ()

-- Toy example SMTLIB string
smtLines :: [ String ]
smtLines = [ "(set-option :print-success true)"
           , "(set-option :produce-models true)"
           , "(declare-const x Int)"
           , "(assert (< x 5))"
           , "(declare-const y Int)"
           , "(assert (= x y))"
           , "(push 1)"
           , "(assert (> y 5))"
           , "(check-sat)"
           , "(pop 1)"
           , "(push 1)"
           , "(assert (> y 3))"
           , "(check-sat)"
           , "(get-value (x y))"
           , "(pop 1)"
           , "(exit)" ]

script :: Z3 [String]
script = traverse evalSMTLib2String smtLines

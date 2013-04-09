
module Main where

import Control.Applicative ( (<$>) )
import qualified Data.Traversable as T

import Z3.Lang

script :: Z3 (Maybe [Integer])
script = do
  -- the ith-queen is in the ith-row.
  -- qi is the column of the ith-queen
  q1 <- var
  q2 <- var
  q3 <- var
  q4 <- var
  assert $ 1 <=* q1 &&* q1 <=* 4
  assert $ 1 <=* q2 &&* q2 <=* 4
  assert $ 1 <=* q3 &&* q3 <=* 4
  assert $ 1 <=* q4 &&* q4 <=* 4
  -- different columns
  assert $ distinct [q1,q2,q3,q4]
  -- avoid diagonal attacks
  assert $ not_ $ diagonal 1 q1 q2
  assert $ not_ $ diagonal 2 q1 q3
  assert $ not_ $ diagonal 3 q1 q4
  assert $ not_ $ diagonal 1 q2 q3
  assert $ not_ $ diagonal 2 q2 q4
  assert $ not_ $ diagonal 1 q3 q4
  -- check and get solution
  checkModel $ evalT [q1,q2,q3,q4]
  where diagonal d c c' = abs(c'-c) ==* fromInteger d

main = evalZ3 script >>= print
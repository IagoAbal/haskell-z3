{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Example.Monad.LinearProgramming
  ( run )
  where

import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Data.Maybe
import Text.Printf

import Debug.Trace

import Z3.Monad

-- Credits to 18.310A Principles of Discrete Applied Mathematics by Prof Michel Goemans at MIT
-- SEE: "Lecture notes on linear programming.", https://math.mit.edu/~goemans/18310S15/18310A.html
run = evalZ3 $ do
  {-
    Let
      x_t = number of tables made per week
      x_c = number of chairs made per week

    Constraints
    total work time:  6 x_t + 3 x_c <= 40
    customer demand:  x_c >= 3 x_t
    storage space:    (x_c / 4) + x_t <= 4

    all variables are non-negative: x_t, x_c >= 0

    Objective:        maximise P(x_t, x_c)
    where
      profit P(x_t, x_c) = 30 x_t + 10 x_c

    Solution
    x_c = 10.667, x_t = 1.333 and the corresponding profit = 146.667
   -}
  [zero, three, four, six, ten, thirty, fourty] <- mapM mkRealNum [0.0, 3.0, 4.0, 6.0, 10.0, 30.0, 40.0]
  x_t <- mkFreshRealVar "x_t"
  x_c <- mkFreshRealVar "x_c"

  -- non-negative constraints
  optimizeAssert =<< mkGe x_t zero
  optimizeAssert =<< mkGe x_c zero

  -- total work time
  optimizeAssert =<< mkGe fourty =<< mkAdd =<< sequence [mkMul [six, x_t], mkMul [three, x_c]]

  -- customer demand
  optimizeAssert =<< mkGe x_c =<< mkMul [three, x_t]

  -- storage space
  optimizeAssert =<< mkGe four =<< mkAdd =<< (:[x_t]) <$> mkDiv x_c four

  -- objective
  objective <- mkAdd =<< sequence [mkMul [thirty, x_t], mkMul [ten, x_c]]
  -- specify minimization
  optimizeMaximize objective

  optimizeCheck [] >>= \case
    Sat -> do
      model <- optimizeGetModel
      profit :: Double <- fromRational <$> fromJust <$> evalReal model objective
      tables :: Double <- fromRational <$> fromJust <$> evalReal model x_t
      chairs :: Double <- fromRational <$> fromJust <$> evalReal model x_c
      liftIO $ printf "tables (x_t): %f, chairs (x_c): %f, profit: %f" tables chairs profit
    Unsat -> do
      traceShowM =<< optimizeGetUnsatCore
      error "Unsat"
    _ -> do
      error <$> optimizeGetReasonUnknown



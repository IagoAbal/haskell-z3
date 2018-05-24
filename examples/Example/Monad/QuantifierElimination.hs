module Example.Monad.QuantifierElimination
  ( run )
  where

import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Z3.Monad

run = evalZ3 $ do
  a <- mkFreshIntVar "a"
  b <- mkFreshIntVar "b"
  c <- mkFreshIntVar "c"

  a' <- toApp a
  b' <- toApp b
  c' <- toApp c

  f1 <- mkExistsConst [] [c'] =<< mkAnd =<< sequence [ mkDistinct [a, b, c], mkEq a =<< mkAdd [b, c] ]
  f2 <- mkForallConst [] [a', b'] f1

  forM_ [f1, f2] $ \f -> do
    g <- mkGoal True True False
    goalAssert g f
    qe  <- mkTactic "qe"  -- eliminate quantifiers
    aig <- mkTactic "aig" -- optionally also simplify
    t <- andThenTactic qe aig
    a <- applyTactic t g
    liftIO . putStrLn =<< astToString =<< mkAnd =<< getGoalFormulas =<< getApplyResultSubgoal a 0

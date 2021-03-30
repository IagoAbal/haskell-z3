module Z3.Regression
  ( spec )
  where

import Test.Hspec

import Control.Monad(forM_)
import Control.Monad.IO.Class

import qualified Z3.Base as Z3
import qualified Z3.Monad

spec :: Spec
spec = do
  describe "issue#23: Crash on parseSMTLib2String" $ do
    it "should not crash" $
      Z3.Monad.evalZ3 issue23script `shouldReturn` Z3.Monad.Unsat

  describe "issue#27: non-deterministic crashes during parallel GC" $ do
    it "should not crash" $
      issue27script `shouldReturn` ()

  describe "issue#29: evalBv" $ do
    it "should correctly evaluate example" $
      Z3.Monad.evalZ3 issue29script `shouldReturn` Just 35

issue23script :: Z3.Monad.Z3 Z3.Monad.Result
issue23script = do
  asts <- Z3.Monad.parseSMTLib2String "(assert (= 1 2))" [] [] [] []
  forM_ asts $ \ast -> do
    Z3.Monad.assert ast
  Z3.Monad.check

issue29script :: Z3.Monad.Z3 (Maybe Integer)
issue29script = do
  i32sort <- Z3.Monad.mkBvSort 32
  let mkV name = do sym <- Z3.Monad.mkStringSymbol name
                    Z3.Monad.mkVar sym i32sort

  c <- Z3.Monad.mkBitvector 32 35
  x <- mkV "x"

  -- Perform some operations on the values
  e  <- Z3.Monad.mkEq c x

  Z3.Monad.assert e

  z3result <- Z3.Monad.solverCheckAndGetModel
  case z3result of
    (Z3.Monad.Sat, Just model) -> Z3.Monad.evalBv True model x
    _ -> return Nothing

issue27script :: IO ()
issue27script = do
  cfg <- Z3.mkConfig
  ctx <- Z3.mkContext cfg
  int <- Z3.mkIntSort ctx
  forM_ [1..100000] $ \i -> do
    Z3.mkNumeral ctx (show i) int

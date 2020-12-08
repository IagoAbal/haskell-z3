module Z3.Regression
  ( spec )
  where

import Test.Hspec

import Control.Monad(forM_)

import qualified Z3.Base as Z3
import qualified Z3.Monad

spec :: Spec
spec = do
  describe "issue#23: Crash on parseSMTLib2String" $ do
    it "should not crash" $
      Z3.Monad.evalZ3 issue23script `shouldReturn` Z3.Monad.Unsat

issue23script :: Z3.Monad.Z3 Z3.Monad.Result
issue23script = do
  asts <- Z3.Monad.parseSMTLib2String "(assert (= 1 2))" [] [] [] []
  forM_ asts $ \ast -> do
    Z3.Monad.assert ast
  Z3.Monad.check

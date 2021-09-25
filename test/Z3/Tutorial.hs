module Z3.Tutorial
  ( spec )
  where

import Test.Hspec

import Control.Monad(forM_)
import Control.Monad.IO.Class

import qualified Z3.Base as Z3
import qualified Z3.Monad

spec :: Spec
spec = do
  describe "Section: Basic Commands" $ do
    it "Basic function definition" $
      (fst <$> Z3.Monad.evalZ3 basicFunctionDefinition) `shouldReturn` Z3.Monad.Sat
  describe "Section: Strategies" $ do
    describe "Section: Introduction" $ do
      it "Apply simplify and solve-eq tactics" $
        Z3.Monad.evalZ3 simplifySolveEqs `shouldReturn` "(goals\n(goal\n  (not (<= y (- 2.0)))\n  (not (<= y 0.0)))\n)"
      it "Apply split-clause tactic" $
        Z3.Monad.evalZ3 applySplitClause `shouldReturn` "(goals\n(goal\n  (< x 0.0)\n  (= x (+ y 1.0))\n  (< y 0.0))\n(goal\n  (> x 0.0)\n  (= x (+ y 1.0))\n  (< y 0.0))\n)"

{-
  (declare-const a String)
  (declare-const b String)
  (assert (= (str.++ b a) (str.++ "abc" b)))
  (check-sat)
  (get-model)
-}
stringConcatentation :: Z3.Monad.Z3 String
stringConcatentation = do
  s <- Z3.Monad.mkStringSort
  asym <- Z3.Monad.mkStringSymbol "a"
  bsym <- Z3.Monad.mkStringSymbol "b"
  a <- Z3.Monad.mkConst asym s
  b <- Z3.Monad.mkConst bsym s
  abcLit <- Z3.Monad.mkString "abc"
  lhs <- Z3.Monad.mkSeqConcat [b,a]
  rhs <- Z3.Monad.mkSeqConcat [abcLit, b]
  Z3.Monad.assert =<< Z3.Monad.mkEq lhs rhs
  (Z3.Sat, Just m) <- Z3.Monad.solverCheckAndGetModel
  Z3.Monad.modelToString m

{-
  (declare-const a Int)
  (declare-fun f (Int Bool) Int)
  (assert (> a 10))
  (assert (< (f a true) 100))
  (check-sat)
-}
basicFunctionDefinition :: Z3.Monad.Z3 (Z3.Monad.Result, Maybe Z3.Monad.Model)
basicFunctionDefinition = do
  b <- Z3.Monad.mkBoolSort
  i <- Z3.Monad.mkIntSort
  asym <- Z3.Monad.mkStringSymbol "a"
  a <- Z3.Monad.mkConst asym i
  fsym <- Z3.Monad.mkStringSymbol "f"
  fDecl <- Z3.Monad.mkFuncDecl fsym [i, b] i
  Z3.Monad.assert =<< Z3.Monad.mkGt a =<< Z3.Monad.mkInt 10 i
  Z3.Monad.solverCheckAndGetModel

{-
  (declare-const x Real)
  (declare-const y Real)

  (assert (> x 0.0))
  (assert (> y 0.0))
  (assert (= x (+ y 2.0)))

  (apply (then simplify solve-eqs))
-}
simplifySolveEqs :: Z3.Monad.Z3 String
simplifySolveEqs = do
  real <- Z3.Monad.mkRealSort
  xsym <- Z3.Monad.mkStringSymbol "x"
  ysym <- Z3.Monad.mkStringSymbol "y"
  x <- Z3.Monad.mkConst xsym real
  y <- Z3.Monad.mkConst ysym real
  r0 <- Z3.Monad.mkReal 0 1
  r2 <- Z3.Monad.mkReal 2 1

  goal <- Z3.Monad.mkGoal False False False

  Z3.Monad.goalAssert goal =<< Z3.Monad.mkGt x r0
  Z3.Monad.goalAssert goal =<< Z3.Monad.mkGt y r0
  rhs <- Z3.Monad.mkAdd [y, r2]
  Z3.Monad.goalAssert goal =<< Z3.Monad.mkEq x rhs

  simplify <- Z3.Monad.mkTactic "simplify"
  solveEqs <- Z3.Monad.mkTactic "solve-eqs"
  tactic <- Z3.Monad.andThenTactic simplify solveEqs
  Z3.Monad.applyResultToString =<< Z3.Monad.applyTactic tactic goal


{-
  (declare-const x Real)
  (declare-const y Real)

  (assert (or (< x 0.0) (> x 0.0)))
  (assert (= x (+ y 1.0)))
  (assert (< y 0.0))

  (apply split-clause)
-}
applySplitClause :: Z3.Monad.Z3 String
applySplitClause = do
  real <- Z3.Monad.mkRealSort
  xsym <- Z3.Monad.mkStringSymbol "x"
  ysym <- Z3.Monad.mkStringSymbol "y"
  x <- Z3.Monad.mkConst xsym real
  y <- Z3.Monad.mkConst ysym real
  r0 <- Z3.Monad.mkReal 0 1
  r1 <- Z3.Monad.mkReal 1 1

  goal <- Z3.Monad.mkGoal False False False
  xLt0 <- Z3.Monad.mkLt x r0
  xGt0 <- Z3.Monad.mkGt x r0
  Z3.Monad.goalAssert goal =<< Z3.Monad.mkOr [xLt0, xGt0]
  Z3.Monad.goalAssert goal =<< Z3.Monad.mkEq x =<< Z3.Monad.mkAdd [y, r1]
  Z3.Monad.goalAssert goal =<< Z3.Monad.mkLt y r0

  tactic <- Z3.Monad.mkTactic "split-clause"
  Z3.Monad.applyResultToString =<< Z3.Monad.applyTactic tactic goal

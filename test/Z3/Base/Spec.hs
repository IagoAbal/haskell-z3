-- TODO: Create a context for all the tests, use push; ...; pop to rollback

module Z3.Base.Spec
  ( spec )
  where

import Test.Hspec
import Test.QuickCheck ( property )
import Test.QuickCheck.Monadic

import qualified Z3.Base as Z3

withContext :: ActionWith Z3.Context -> IO ()
withContext k = do
  ctx <- Z3.withConfig Z3.mkContext
  k ctx

anyZ3Error :: Selector Z3.Z3Error
anyZ3Error = const True

spec :: Spec
spec = around withContext $ do

  context "Propositional Logic and Equality" $ do

    specify "mkBool" $ \ctx -> property $ \b ->
      monadicIO $ do
        x::Maybe Bool <- run $ do
          ast <- Z3.mkBool ctx b
          Z3.getBoolValue ctx ast
        assert $ x == Just b

  context "Numerals" $ do

    specify "mkInt" $ \ctx -> property $ \(i :: Integer) ->
      monadicIO $ do
        x::Integer <- run $ do
          ast <- Z3.mkInteger ctx i;
          Z3.getInt ctx ast;
        assert $ x == i

  context "AST Equality and Substitution" $ do
    specify "isEqAST" $ \ctx ->
      monadicIO $ do
        (r1, r2) <- run $ do
          x1 <- Z3.mkFreshIntVar ctx "x1"
          x2 <- Z3.mkFreshIntVar ctx "x2"
          x3 <- Z3.mkFreshIntVar ctx "x3"

          s  <- Z3.mkAdd ctx [x1, x2]
          s' <- Z3.mkAdd ctx [x1, x2]
          s23 <- Z3.mkAdd ctx [x2, x3]

          r1 <- Z3.isEqAST ctx s s'
          r2 <- Z3.isEqAST ctx s s23

          return (r1, r2)
        assert r1
        assert (not r2)

    specify "substitute" $ \ctx ->
      monadicIO $ do
        r <- run $ do
          x1 <- Z3.mkFreshIntVar ctx "x1"
          x2 <- Z3.mkFreshIntVar ctx "x2"
          x3 <- Z3.mkFreshIntVar ctx "x3"
          x4 <- Z3.mkFreshIntVar ctx "x4"

          s12 <- Z3.mkAdd ctx [x1, x2]
          s34 <- Z3.mkAdd ctx [x3, x4]

          s34' <- Z3.substitute ctx s12 [(x1, x3), (x2, x4)]

          Z3.isEqAST ctx s34 s34'
        assert r

  context "Bit-vectors" $ do

    specify "mkBvmul" $ \ctx ->
      let bad = do {
          x <- Z3.mkFreshIntVar ctx "x";
          Z3.mkBvmul ctx x x
        }
        in
      bad `shouldThrow` anyZ3Error

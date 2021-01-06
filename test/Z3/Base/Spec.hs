-- TODO: Create a context for all the tests, use push; ...; pop to rollback

module Z3.Base.Spec
  ( spec )
  where

import Test.Hspec
import Test.QuickCheck ( property, choose )
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Monadic

import qualified Z3.Base as Z3

withContext :: ActionWith Z3.Context -> IO ()
withContext k = do
  ctx <- Z3.withConfig Z3.mkContext
  k ctx

anyZ3Error :: Selector Z3.Z3Error
anyZ3Error = const True

data ValidPowerInput = ValidPowerInput Integer Integer
  deriving Show

instance Arbitrary ValidPowerInput where
  arbitrary = do i <- choose (0::Integer, 10)
                 j <- choose (0::Integer, 64)
                 return $ ValidPowerInput i j

z3powerDef :: Integer -> Integer -> Integer
z3powerDef 0 0 = 0
z3powerDef i j = i ^ j

spec :: Spec
spec = around withContext $ do


  context "Sorts" $ do

    specify "isEqSort" $ \ctx ->
      monadicIO $ do
        assert =<< (run $ do
          s1 <- Z3.mkIntSort ctx
          s2 <- Z3.mkIntSort ctx
          Z3.isEqSort ctx s1 s2)
        assert . not =<< (run $ do
          s1 <- Z3.mkIntSort ctx
          s2 <- Z3.mkRealSort ctx
          Z3.isEqSort ctx s1 s2)
        assert =<< (run $ do
          s1 <- Z3.mkRealSort ctx
          s2 <- Z3.mkRealSort ctx
          Z3.isEqSort ctx s1 s2)
        assert =<< (run $ do
          s1 <- Z3.mkRealSort ctx
          s2 <- Z3.mkRealSort ctx
          s12 <- Z3.mkArraySort ctx s1 s2
          s21 <- Z3.mkArraySort ctx s2 s1
          Z3.isEqSort ctx s21 s12)
        assert . not =<< (run $ do
          s1 <- Z3.mkBoolSort ctx
          s2 <- Z3.mkRealSort ctx
          s12 <- Z3.mkArraySort ctx s1 s2
          s21 <- Z3.mkArraySort ctx s2 s1
          Z3.isEqSort ctx s21 s12)

    specify "getSortId" $ \ctx -> sequence_ [ (do
                                                s1 <- Z3.mkIntSort ctx
                                                s2 <- Z3.mkIntSort ctx
                                                id1 <- Z3.getSortId ctx s1
                                                id2 <- Z3.getSortId ctx s2
                                                return $ id1 == id2) `shouldReturn` True
                                            , (do
                                                s1 <- Z3.mkRealSort ctx
                                                s2 <- Z3.mkIntSort ctx
                                                id1 <- Z3.getSortId ctx s1
                                                id2 <- Z3.getSortId ctx s2
                                                return $ id1 == id2) `shouldReturn` False
                                            , (do
                                                s1 <- Z3.mkIntSort ctx
                                                s2 <- Z3.mkRealSort ctx
                                                id12 <- Z3.getSortId ctx =<< Z3.mkArraySort ctx s1 s2
                                                id21 <- Z3.getSortId ctx =<< Z3.mkArraySort ctx s2 s1
                                                return $ id12 == id21) `shouldReturn` False
                                            ]

    specify "sortToAst" $ \ctx ->
      (do
        s1 <- Z3.mkIntSort ctx
        s2 <- Z3.mkRealSort ctx
        ast <- Z3.sortToAst ctx =<< Z3.mkArraySort ctx s1 s2
        Z3.astToString ctx ast)
      `shouldReturn` "(Array Int Real)"

    specify "getSortName" $ \ctx ->
      (do
        s1 <- Z3.mkIntSort ctx
        Z3.getSymbolString ctx =<< Z3.getSortName ctx s1)
      `shouldReturn` "Int"

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

    specify "mkPower" $ \ctx -> property $ \(ValidPowerInput i j) ->
      monadicIO $ do
        x <- run $ do
          iAst <- Z3.mkInteger ctx i
          jAst <- Z3.mkInteger ctx j
          p <- Z3.mkPower ctx iAst jAst
          sol <- Z3.mkSolver ctx
          (_, Just model) <- Z3.solverCheckAndGetModel ctx sol
          Z3.evalInt ctx model p 
        assert $ x == Just (z3powerDef i j)

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

    specify "evalBv" $ \ctx -> property $ \(i :: Integer) ->
      monadicIO $ do
        x <- run $ do
          ast <- Z3.mkBitvector ctx 32 i
          solver <- Z3.mkSolver ctx
          (_, Just model) <- Z3.solverCheckAndGetModel ctx solver
          Z3.evalBv ctx True model ast
        assert $ x == Just i

  context "Quantifiers" $ do

    specify "mkForallW" $ \ctx ->
      (do
        int <- Z3.mkIntSort ctx
        x <- Z3.mkStringSymbol ctx "x"
        fa <- Z3.mkForallW ctx 554 [] [x] [int] =<< Z3.mkBool ctx True
        Z3.getQuantifierWeight ctx fa
      ) `shouldReturn` 554

    specify "mkForall" $ \ctx ->
      (do
        int <- Z3.mkIntSort ctx
        x <- Z3.mkStringSymbol ctx "x"
        fa <- Z3.mkForall ctx [] [x] [int] =<< Z3.mkBool ctx True
        Z3.getQuantifierWeight ctx fa
      ) `shouldReturn` 0

    specify "mkExistsW" $ \ctx ->
      (do
        int <- Z3.mkIntSort ctx
        x <- Z3.mkStringSymbol ctx "x"
        fa <- Z3.mkExistsW ctx 37 [] [x] [int] =<< Z3.mkBool ctx True
        Z3.getQuantifierWeight ctx fa
      ) `shouldReturn` 37

    specify "mkExists" $ \ctx ->
      (do
        int <- Z3.mkIntSort ctx
        x <- Z3.mkStringSymbol ctx "x"
        fa <- Z3.mkExists ctx [] [x] [int] =<< Z3.mkBool ctx True
        Z3.getQuantifierWeight ctx fa
      ) `shouldReturn` 0

    specify "mkForallWConst" $ \ctx ->
      (do
        int <- Z3.mkIntSort ctx
        x <- Z3.mkStringSymbol ctx "x"
        v <- Z3.toApp ctx =<< Z3.mkConst ctx x int
        fa <- Z3.mkForallWConst ctx 554 [] [v] =<< Z3.mkBool ctx True
        Z3.getQuantifierWeight ctx fa
      ) `shouldReturn` 554

    specify "mkForallConst" $ \ctx ->
      (do
        int <- Z3.mkIntSort ctx
        x <- Z3.mkStringSymbol ctx "x"
        v <- Z3.toApp ctx =<< Z3.mkConst ctx x int
        fa <- Z3.mkForallConst ctx [] [v] =<< Z3.mkBool ctx True
        Z3.getQuantifierWeight ctx fa
      ) `shouldReturn` 0

    specify "mkExistsWConst" $ \ctx ->
      (do
        int <- Z3.mkIntSort ctx
        x <- Z3.mkStringSymbol ctx "x"
        v <- Z3.toApp ctx =<< Z3.mkConst ctx x int
        fa <- Z3.mkExistsWConst ctx 991 [] [v] =<< Z3.mkBool ctx True
        Z3.getQuantifierWeight ctx fa
      ) `shouldReturn` 991

    specify "mkExistsConst" $ \ctx ->
      (do
        int <- Z3.mkIntSort ctx
        x <- Z3.mkStringSymbol ctx "x"
        v <- Z3.toApp ctx =<< Z3.mkConst ctx x int
        fa <- Z3.mkExistsConst ctx [] [v] =<< Z3.mkBool ctx True
        Z3.getQuantifierWeight ctx fa
      ) `shouldReturn` 0

  context "Tuple sorts" $ do

    specify "mkTupleSort" $ \ctx -> property $ \(i::Integer) (b1v::Bool) (b2v::Bool) ->
      (do
        -- create a new tuple sort of the form (f1:int, f2:bool, f3:bool)
        name <- Z3.mkStringSymbol ctx "myTuple"
        f1 <- Z3.mkStringSymbol ctx "f1"
        f2 <- Z3.mkStringSymbol ctx "f2"
        f3 <- Z3.mkStringSymbol ctx "f3"
        int <- Z3.mkIntSort ctx
        bool <- Z3.mkBoolSort ctx
        (_, cons, [f1proj, f2proj, f3proj]) <- Z3.mkTupleSort ctx name [(f1, int), (f2, bool), (f3, bool)]

        one <- Z3.mkIntNum ctx i
        b1 <- Z3.mkBool ctx b1v
        b2 <- Z3.mkBool ctx b2v

        tuple <- Z3.mkApp ctx cons [one,b1,b2]

        sol <- Z3.mkSolver ctx
        (_, Just model) <- Z3.solverCheckAndGetModel ctx sol

        x1 <- Z3.evalInt ctx model =<< Z3.mkApp ctx f1proj [tuple]
        x2 <- Z3.evalBool ctx model =<< Z3.mkApp ctx f2proj [tuple]
        x3 <- Z3.evalBool ctx model =<< Z3.mkApp ctx f3proj [tuple]
        return (x1,x2,x3)
      ) `shouldReturn` (Just i, Just b1v, Just b2v)

    specify "mkTupleType, mkTuple, mkIndexTuple, mkProjTuple" $ \ctx -> property $ \(i::Integer) (b1v::Bool) (b2v::Bool) ->
      (do
        -- create a new tuple sort of the form (f1:int, f2:bool, f3:bool)
        name <- Z3.mkStringSymbol ctx "myTuple"
        int <- Z3.mkIntSort ctx
        bool <- Z3.mkBoolSort ctx
        tupleType <- Z3.mkTupleType ctx name [("f1", int), ("f2", bool), ("f3", bool)]

        one <- Z3.mkIntNum ctx i
        b1 <- Z3.mkBool ctx b1v
        b2 <- Z3.mkBool ctx b2v

        tuple <- Z3.mkTuple ctx tupleType [one,b1,b2]

        sol <- Z3.mkSolver ctx
        (_, Just model) <- Z3.solverCheckAndGetModel ctx sol

        x1 <- Z3.evalInt ctx model =<< Z3.mkIndexTuple ctx tupleType 0 tuple
        x1' <- Z3.evalInt ctx model =<< Z3.mkProjTuple ctx tupleType "f1" tuple
        x2 <- Z3.evalBool ctx model =<< Z3.mkIndexTuple ctx tupleType 1 tuple
        x2' <- Z3.evalBool ctx model =<< Z3.mkProjTuple ctx tupleType "f2" tuple
        x3 <- Z3.evalBool ctx model =<< Z3.mkIndexTuple ctx tupleType 2 tuple
        x3' <- Z3.evalBool ctx model =<< Z3.mkProjTuple ctx tupleType "f3" tuple
        return (x1,x1',x2,x2',x3,x3')
      ) `shouldReturn` (Just i, Just i, Just b1v, Just b1v, Just b2v, Just b2v)



    specify "mkTupleSort nested" $ \ctx -> property $ \(i::Integer) ->
      (do
        -- create a new tuple sort T1 of the form (f1:int)
        t1 <- Z3.mkStringSymbol ctx "T1"
        f1 <- Z3.mkStringSymbol ctx "f1"
        int <- Z3.mkIntSort ctx
        (t1sort, t1cons, [f1proj]) <- Z3.mkTupleSort ctx t1 [(f1, int)]

        -- create a new tuple sort T2 of the form (f2:T1)
        t2 <- Z3.mkStringSymbol ctx "T2"
        f2 <- Z3.mkStringSymbol ctx "f2"

        (_, t2cons, [f2proj]) <- Z3.mkTupleSort ctx t2 [(f2, t1sort)]

        x <- Z3.mkIntNum ctx i

        let mkApp' c f arg = Z3.mkApp c f [arg]

        (_, Just model) <- Z3.solverCheckAndGetModel ctx =<< Z3.mkSolver ctx

        Z3.evalInt ctx model =<< mkApp' ctx f1proj =<< mkApp' ctx f2proj =<< mkApp' ctx t2cons =<< Z3.mkApp ctx t1cons [x]
      ) `shouldReturn` Just i

-- TODO: Create a context for all the tests, use push; ...; pop to rollback

module Z3.Base.Spec
  ( spec )
  where

import Data.Maybe (fromJust)
import Test.Hspec
import Test.QuickCheck ( property, choose, (==>) )
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Monadic

import Data.Word(Word64)

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

  context "Floating point" $ do

    specify "algebraicIsValue" $ \ctx ->
      (do
        ds <- Z3.mkFpaSortSingle ctx
        xsym <- Z3.mkStringSymbol ctx "x"
        xvar <- Z3.mkVar ctx xsym ds
        rm <- Z3.mkFpaRtn ctx
        sol <- Z3.mkSolver ctx
        x <- Z3.mkFpaNumeralFloat ctx 1099511627776 ds
        z <- Z3.mkFpaEq ctx xvar =<< (Z3.mkFpaAdd ctx rm x x)
        Z3.solverAssertCnstr ctx sol z
        (Z3.Sat,Just m) <- Z3.solverCheckAndGetModel ctx sol
        Just r <- Z3.modelEval ctx m xvar False
        sign <- Z3.fpaIsNumeralPositive ctx r
        significand <- Z3.fpaGetNumeralSignificandString ctx r
        exponent <- Z3.fpaGetNumeralExponentString ctx r False
        return (sign, significand, exponent)
        {-
        This encodes the constraint 'x = 1099511627776 + 1099511627776'.
        Since 1099511627776 = 2^40, the result is 2^41 and thus the significand 1 and exponent 41.
        -}
      ) `shouldReturn` (True, "1", "41")

  context "Algebraic Numbers" $ do

    specify "algebraicIsValue" $ \ctx ->
      (do
        x <- Z3.mkReal ctx 10 1
        Z3.algebraicIsValue ctx x
      ) `shouldReturn` True

    specify "algebraicMul" $ \ctx ->
      (do
        i10_3 <- Z3.mkReal ctx 10 3
        i5_8 <- Z3.mkReal ctx 5 8
        r <- Z3.algebraicMul ctx i10_3 i5_8
        Z3.getNumeralString ctx r
      ) `shouldReturn` "25/12"

    specify "getNumeralStringAlgebraic" $ \ctx ->
      (do
        sol <- Z3.mkSolver ctx
        two <- Z3.mkReal ctx 2 1
        zero <- Z3.mkReal ctx 0 1
        rs <- Z3.mkRealSort ctx
        xsym <- Z3.mkStringSymbol ctx "x"
        xvar <- Z3.mkVar ctx xsym rs
        x2 <- Z3.mkMul ctx [xvar, xvar]
        Z3.solverAssertCnstr ctx sol =<< Z3.mkEq ctx x2 two
        Z3.solverAssertCnstr ctx sol =<< Z3.mkGe ctx xvar zero
        (Z3.Sat, Just m) <- Z3.solverCheckAndGetModel ctx sol
        Just r <- Z3.modelEval ctx m xvar False
        astKind <- Z3.getAstKind ctx r
        isNumeral <- Z3.isNumeralAst ctx r
        isAlgebraic <- Z3.isAlgebraicNumber ctx r
        decString <- Z3.getNumeralDecimalString ctx r 5
        lowerAst <- Z3.getAlgebraicNumberLower ctx r 10
        lowerDouble <- Z3.getNumeralDouble ctx lowerAst
        upperAst <- Z3.getAlgebraicNumberUpper ctx r 10
        upperDouble <- Z3.getNumeralDouble ctx upperAst
        return (astKind, isNumeral, isAlgebraic, decString, lowerDouble, upperDouble)
      ) `shouldReturn` (Z3.Z3_APP_AST, True, True, "1.41421?", 1.414213562372879, 1.4142135623733338)

  context "Global Parameters" $ do
    specify "globalParamSet / globalParamGet" $ \_ ->
      (do
        Z3.globalParamSet "memory_max_size" "10000"
        beforeReset <- Z3.globalParamGet "memory_max_size"
        Z3.globalParamResetAll
        afterReset <- Z3.globalParamGet "memory_max_size"
        invalid <- Z3.globalParamGet "invalid_option"
        return (beforeReset, afterReset, invalid)
      ) `shouldReturn` (Just "10000", Just "0", Nothing)

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

    specify "mkArraySortN" $ \ctx ->
      (do
        s1 <- Z3.mkIntSort ctx
        s2 <- Z3.mkBoolSort ctx
        s3 <- Z3.mkRealSort ctx
        aSort <- Z3.mkArraySortN ctx [s1,s2] s3
        Z3.getSymbolString ctx =<< Z3.getSortName ctx aSort)
      `shouldReturn` "Array"

    specify "mkEnumerationSort" $ \ctx ->
      (do
        a <- Z3.mkStringSymbol ctx "A"
        b <- Z3.mkStringSymbol ctx "B"
        c <- Z3.mkStringSymbol ctx "C"
        s <- Z3.mkStringSymbol ctx "S"
        (eSort, consts, tests) <- Z3.mkEnumerationSort ctx s [a,b,c]
        let [aCon, bCon, cCon] = consts
            [aTest, bTest, cTest] = tests
        solver <- Z3.mkSolver ctx
        aConst <- Z3.mkApp ctx aCon []
        Z3.solverAssertCnstr ctx solver =<< Z3.mkApp ctx aTest [aConst]
        bConst <- Z3.mkApp ctx bCon []
        Z3.solverAssertCnstr ctx solver =<< Z3.mkApp ctx bTest [bConst]
        cConst <- Z3.mkApp ctx cCon []
        Z3.solverAssertCnstr ctx solver =<< Z3.mkApp ctx cTest [cConst]
        Z3.solverAssertCnstr ctx solver =<< Z3.mkNot ctx =<< Z3.mkApp ctx aTest [cConst]
        r <- Z3.solverCheck ctx solver
        sName <- Z3.getSymbolString ctx =<< Z3.getSortName ctx eSort
        return (sName, length consts, length tests, r))
      `shouldReturn` ("S", 3, 3, Z3.Sat)

    specify "mkListSort" $ \ctx ->
      (do
        name <- Z3.mkStringSymbol ctx "intList"
        int <- Z3.mkIntSort ctx
        (lSort, nil, isNil, cons, isCons, lHead, lTail) <- Z3.mkListSort ctx name int
        i1 <- Z3.mkIntNum ctx 1
        emp <- Z3.mkApp ctx nil []
        sing <- Z3.mkApp ctx cons [i1, emp]
        t <- Z3.mkApp ctx lTail [sing]
        solver <- Z3.mkSolver ctx
        Z3.solverAssertCnstr ctx solver =<< Z3.mkNot ctx =<< Z3.mkEq ctx t emp
        r <- Z3.solverCheck ctx solver
        sName <- Z3.getSymbolString ctx =<< Z3.getSortName ctx lSort
        return (sName, r))
      `shouldReturn` ("intList", Z3.Unsat)

  context "Propositional Logic and Equality" $ do

    specify "mkBool" $ \ctx -> property $ \b ->
      monadicIO $ do
        x::Maybe Bool <- run $ do
          ast <- Z3.mkBool ctx b
          Z3.getBoolValue ctx ast
        assert $ x == Just b

    specify "mkAtMost0" $ \ctx ->
      (do
        a <- Z3.mkFreshBoolVar ctx "A"
        b <- Z3.mkFreshBoolVar ctx "B"
        c <- Z3.mkFreshBoolVar ctx "C"
        solver <- Z3.mkSolver ctx
        Z3.solverAssertCnstr ctx solver =<< Z3.mkAtMost ctx [a, b, c] 0
        (_r, Just model) <- Z3.solverCheckAndGetModel ctx solver
        mapM (Z3.evalBool ctx model) [a, b, c])
      `shouldReturn` [Just False, Just False, Just False]

    specify "mkAtMost2" $ \ctx ->
      (do
        a <- Z3.mkFreshBoolVar ctx "A"
        b <- Z3.mkFreshBoolVar ctx "B"
        c <- Z3.mkFreshBoolVar ctx "C"
        solver <- Z3.mkSolver ctx
        Z3.solverAssertCnstr ctx solver =<< Z3.mkAtMost ctx [a, b, c] 2
        Z3.solverAssertCnstr ctx solver =<< Z3.mkAnd ctx [a, b, c]
        Z3.solverCheck ctx solver)
      `shouldReturn` Z3.Unsat

    specify "mkAtMost4" $ \ctx ->
      (do
        a <- Z3.mkFreshBoolVar ctx "A"
        b <- Z3.mkFreshBoolVar ctx "B"
        c <- Z3.mkFreshBoolVar ctx "C"
        solver <- Z3.mkSolver ctx
        Z3.solverAssertCnstr ctx solver =<< Z3.mkAtMost ctx [a, b, c] 4
        Z3.solverCheck ctx solver)
      `shouldReturn` Z3.Sat

    specify "mkAtLeast0" $ \ctx ->
      (do
        a <- Z3.mkFreshBoolVar ctx "A"
        b <- Z3.mkFreshBoolVar ctx "B"
        c <- Z3.mkFreshBoolVar ctx "C"
        solver <- Z3.mkSolver ctx
        Z3.solverAssertCnstr ctx solver =<< Z3.mkAtLeast ctx [a, b, c] 0
        Z3.solverCheck ctx solver)
      `shouldReturn` Z3.Sat

    specify "mkAtLeast2" $ \ctx ->
      (do
        a <- Z3.mkFreshBoolVar ctx "A"
        b <- Z3.mkFreshBoolVar ctx "B"
        c <- Z3.mkFreshBoolVar ctx "C"
        solver <- Z3.mkSolver ctx
        Z3.solverAssertCnstr ctx solver =<< Z3.mkAtLeast ctx [a, b, c] 2
        Z3.solverCheck ctx solver)
      `shouldReturn` Z3.Sat

    specify "mkAtLeast4" $ \ctx ->
      (do
        a <- Z3.mkFreshBoolVar ctx "A"
        b <- Z3.mkFreshBoolVar ctx "B"
        c <- Z3.mkFreshBoolVar ctx "C"
        solver <- Z3.mkSolver ctx
        Z3.solverAssertCnstr ctx solver =<< Z3.mkAtLeast ctx [a, b, c] 4
        Z3.solverCheck ctx solver)
      `shouldReturn` Z3.Unsat

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

    specify "getNumeralInteger" $ \ctx ->
      (do
        n <- Z3.mkIntNum ctx (42 :: Int)
        binary <- Z3.getNumeralBinaryString ctx n
        return (binary)
      ) `shouldReturn` ("101010")

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
        (myTuple, cons, [f1proj, f2proj, f3proj]) <- Z3.mkTupleSort ctx name [(f1, int), (f2, bool), (f3, bool)]

        correctDecl <- (== cons) <$> Z3.getTupleSortMkDecl ctx myTuple
        correctNumFields <- (== 3) <$> Z3.getTupleSortNumFields ctx myTuple
        correctGetFieldDecl <- (== f2proj) <$> Z3.getTupleSortFieldDecl ctx myTuple 1

        one <- Z3.mkIntNum ctx i
        b1 <- Z3.mkBool ctx b1v
        b2 <- Z3.mkBool ctx b2v

        tuple <- Z3.mkApp ctx cons [one,b1,b2]

        sol <- Z3.mkSolver ctx
        (_, Just model) <- Z3.solverCheckAndGetModel ctx sol

        x1 <- Z3.evalInt ctx model =<< Z3.mkApp ctx f1proj [tuple]
        x2 <- Z3.evalBool ctx model =<< Z3.mkApp ctx f2proj [tuple]
        x3 <- Z3.evalBool ctx model =<< Z3.mkApp ctx f3proj [tuple]
        return (correctDecl, correctNumFields, correctGetFieldDecl, x1,x2,x3)

      ) `shouldReturn` (True, True, True, Just i, Just b1v, Just b2v)

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

  context "Finite Domain Sorts" $ do

    specify "mkFiniteDomainSort, getFiniteDomainSortSize" $ \ctx -> property $ \(i::Word64) ->
      -- Z3 does not allow empty sorts
      (i /= 0 ==>
        (do
          name <- Z3.mkStringSymbol ctx "name"
          s <- Z3.mkFiniteDomainSort ctx name i
          Z3.getFiniteDomainSortSize ctx s
        ) `shouldReturn` Just i)

  context "Function Declarations" $ do

    specify "getArity" $ \ctx ->
      (do
        fun <- Z3.mkStringSymbol ctx "fun"
        int <- Z3.mkIntSort ctx
        bool <- Z3.mkBoolSort ctx
        fdecl <- Z3.mkFuncDecl ctx fun [int, int, int] bool
        arity <- Z3.getArity ctx fdecl
        return arity
      ) `shouldReturn` 3

  context "Tactics" $ do

    specify "mkTactic, applyTactic, repeatTactic, applyResultToString" $ \ctx ->
      (do real <- Z3.mkRealSort ctx
          xsym <- Z3.mkStringSymbol ctx "x"
          ysym <- Z3.mkStringSymbol ctx "y"
          x <- Z3.mkConst ctx xsym real
          y <- Z3.mkConst ctx ysym real
          r0 <- Z3.mkReal ctx 0 1
          r2 <- Z3.mkReal ctx 2 1

          goal <- Z3.mkGoal ctx False False False
          xLt0 <- Z3.mkLt ctx x r0
          xGt0 <- Z3.mkGt ctx x r0
          Z3.goalAssert ctx goal =<< Z3.mkOr ctx [xLt0, xGt0]
          Z3.goalAssert ctx goal =<< Z3.mkEq ctx x =<< Z3.mkAdd ctx [y, r2]
          Z3.goalAssert ctx goal =<< Z3.mkLt ctx y r0

          simplify <- Z3.mkTactic ctx "solve-eqs"
          tactic <- Z3.repeatTactic ctx simplify 10
          Z3.applyResultToString ctx =<< Z3.applyTactic ctx tactic goal)
          `shouldReturn` "(goals\n(goal\n  (or (not (<= (- 2.0) y)) (not (<= y (- 2.0))))\n  (not (<= 0.0 y)))\n)"


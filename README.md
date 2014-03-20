
# Haskell bindings for Microsoft's Z3 (unofficial)

These are Haskell bindings for the Z3 theorem prover.

We offer several interfaces to the Z3 API, the most widely used is Z3.Monad, here is an example script that solves the 4-queen puzzle:

    script :: Z3 (Maybe [Integer])
    script = do
      intSort <- mkIntSort
      q1 <- flip mkConst intSort =<< mkStringSymbol "q1"
      q2 <- flip mkConst intSort =<< mkStringSymbol "q2"
      q3 <- flip mkConst intSort =<< mkStringSymbol "q3"
      q4 <- flip mkConst intSort =<< mkStringSymbol "q4"
      _1 <- mkInt 1
      _4 <- mkInt 4
      -- the ith-queen is in the ith-row.
      -- qi is the column of the ith-queen
      assertCnstr =<< mkAnd =<< T.sequence [mkLe _1 q1, mkLe q1 _4]
      assertCnstr =<< mkAnd =<< T.sequence [mkLe _1 q2, mkLe q2 _4]
      assertCnstr =<< mkAnd =<< T.sequence [mkLe _1 q3, mkLe q3 _4]
      assertCnstr =<< mkAnd =<< T.sequence [mkLe _1 q4, mkLe q4 _4]
      -- different columns
      assertCnstr =<< mkDistinct [q1,q2,q3,q4]
      -- avoid diagonal attacks
      assertCnstr =<< mkNot =<< diagonal 1 q1 q2
      assertCnstr =<< mkNot =<< diagonal 2 q1 q3
      assertCnstr =<< mkNot =<< diagonal 3 q1 q4
      assertCnstr =<< mkNot =<< diagonal 1 q2 q3
      assertCnstr =<< mkNot =<< diagonal 2 q2 q4
      assertCnstr =<< mkNot =<< diagonal 1 q3 q4
      -- check and get solution
      fmap snd $ withModel $ \m -> do
        mb_cs <- evalT m [q1,q2,q3,q4]
        mapM getInt $ fromJust mb_cs
      where mkAbs :: AST -> Z3 AST
            mkAbs x = do
              _0 <- mkInt 0
              join $ mkIte <$> mkLe _0 x <*> pure x <*> mkUnaryMinus x
            diagonal d c c' =
              join $ mkEq <$> (mkAbs =<< mkSub [c',c]) <*> (mkInt d)

## Installation

Preferably use the [z3](http://hackage.haskell.org/package/z3) package.

* Download and install a **stable** [Z3](http://z3.codeplex.com) release.
  We don't test this package with unstable versions.
* Just type _cabal install z3_ if you used the standard locations for dynamic libraries (_/usr/lib_) and header files (_/usr/include_).

    * Otherwise use the _--extra-lib-dirs_ and _--extra-include-dirs_ Cabal flags when installing.

## Contributing

Some modules include a HACKING documentation section at the top that you should read before.

We appreciate your contact before contributing to the project, in order to avoid duplication of work and agree on possible design decisions.

Please follow roughly the same coding style than us.

### Adding support an API function

1. Declare the functions in Z3/Lang/C.hsc.
1. Lift the function to the IO monad in Z3/Base.hs, there is a bunch of marshalling helpers that should make this trivial in most cases.
1. Lift the function to the Z3 monad in Z3/Monad.hs, this is trivially done using one of the _liftFun_ helpers.


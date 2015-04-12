
# Haskell bindings for Microsoft's Z3 (unofficial)

These are Haskell bindings for the Z3 theorem prover.
We don't provide any high-level interface (e.g. in the form of a Haskell eDSL) here,
these bindings are targeted to those who want to build verification tools on top of Z3 in Haskell.

[Changelog here.](https://bitbucket.org/iago/z3-haskell/src/tip/CHANGES.md)

[Examples here.](https://bitbucket.org/iago/z3-haskell/src/tip/examples)

[Do you want to contribute?](https://bitbucket.org/iago/z3-haskell/src/tip/HACKING.md)

## Installation

Preferably use the [z3](http://hackage.haskell.org/package/z3) package.

* Install a [Z3](https://github.com/Z3Prover/z3) *4.x* release.
  (Support for Z3 *3.x* is provided by the *0.3.2* version of these bindings.)
* Just type _cabal install z3_ if you used the standard locations for dynamic libraries (_/usr/lib_) and header files (_/usr/include_).

    * Otherwise use the _--extra-lib-dirs_ and _--extra-include-dirs_ Cabal flags when installing.

## Example

Most people uses the ```Z3.Monad``` interface.
Here is an example script that solves the 4-queen puzzle:

    import Control.Applicative
    import Control.Monad ( join )
    import Data.Maybe
    import qualified Data.Traversable as T

    import Z3.Monad

    script :: Z3 (Maybe [Integer])
    script = do
      q1 <- mkFreshIntVar "q1"
      q2 <- mkFreshIntVar "q2"
      q3 <- mkFreshIntVar "q3"
      q4 <- mkFreshIntVar "q4"
      _1 <- mkInteger 1
      _4 <- mkInteger 4
      -- the ith-queen is in the ith-row.
      -- qi is the column of the ith-queen
      assert =<< mkAnd =<< T.sequence
        [ mkLe _1 q1, mkLe q1 _4  -- 1 <= q1 <= 4
        , mkLe _1 q2, mkLe q2 _4
        , mkLe _1 q3, mkLe q3 _4
        , mkLe _1 q4, mkLe q4 _4
        ]
      -- different columns
      assert =<< mkDistinct [q1,q2,q3,q4]
      -- avoid diagonal attacks
      assert =<< mkNot =<< mkOr =<< T.sequence
        [ diagonal 1 q1 q2  -- diagonal line of attack between q1 and q2
        , diagonal 2 q1 q3
        , diagonal 3 q1 q4
        , diagonal 1 q2 q3
        , diagonal 2 q2 q4
        , diagonal 1 q3 q4
        ]
      -- check and get solution
      fmap snd $ withModel $ \m ->
        catMaybes <$> mapM (evalInt m) [q1,q2,q3,q4]
      where mkAbs x = do
              _0 <- mkInteger 0
              join $ mkIte <$> mkLe _0 x <*> pure x <*> mkUnaryMinus x
            diagonal d c c' =
              join $ mkEq <$> (mkAbs =<< mkSub [c',c]) <*> (mkInteger d)

In order to run this SMT script:

    main :: IO ()
    main = evalZ3With Nothing opts script >>= \mbSol ->
            case mbSol of
                 Nothing  -> error "No solution found."
                 Just sol -> putStr "Solution: " >> print sol
      where opts = opt "MODEL" True +? opt "MODEL_COMPLETION" True


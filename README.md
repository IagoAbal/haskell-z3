# Haskell bindings for Microsoft's Z3 (unofficial)

![Version](https://img.shields.io/hackage/v/z3)
![Testsuite workflow](https://github.com/IagoAbal/haskell-z3/actions/workflows/testsuite.yml/badge.svg)

These are Haskell bindings for the Z3 theorem prover.
We don't provide any high-level interface (e.g. in the form of a Haskell eDSL) here,
these bindings are targeted to those who want to build verification tools on top of Z3 in Haskell.

[Changelog here.](CHANGES.md)

[Examples here.](examples)

[Do you want to contribute?](HACKING.md)

## State of maintenance

The library is currently "maintained", meaning that I try to be responsive to new
issues and pull requests.
Unfortunately I do not have time to investigate issues or to do major work myself.
I do try to help those who want to contribute.

If someone demonstrates willingness to maintain the library more actively
in the long run, then I will be very happy to give the required permissions
to become a co-maintainer.
In the meantime I will do my best to keep it alive.

## Supported versions and version policy

Z3 releases come out often and sometimes introduce backwards incompatible changes.
In order to avoid churn and `#ifdef`-ery, we only support recent releases of the latest Z3 minor version.
We use semantic versioning to reflect which versions are supported:

    <z3-version>.<bindings-version>[.<patch-level>]

The `<z3-version>` indicates which version of Z3 is supported, it is computed as
_x*100+y_ for Z3 _x.y_. For example, versions _408.y.z_ of these bindings are
meant to support versions _4.8.*_ of Z3.
This version policy is in line with Haskell's PVP.
If you are using an older solver version you can check compatibility with these bindings below:

### Z3-4.8.* compatibility

| Bindings version / Z3 version  | 4.8.12  | 4.8.11  | 4.8.10  | 4.8.9   | 4.8.7   | 4.8.6   | 4.8.5   | 4.8.4   | 4.8.3   | 4.8.1   |
| ----              | ---    | ---    | ---    | ---    | ---    | ---    | ---    | ---    | ---    | ---    |
| 408.3             | ✔      | ✔      | ✔      | ✔      | ❌     | ❌     | ❌     | ❌     | ❌     | ❌     |
| 408.2             | ✔      | ✔      | ✔      | ✔      | ✔      | ✔      | ✔      | ❌     | ❌     | ❌     |
| 408.1             | ✔      | ✔      | ✔      | ✔      | ✔      | ✔      | ✔      | ✔      | ✔      | ✔      |
| 408.0             | ❌     | ❌     | ❌     | ❌     | ❌     | ❌     | ❌     | ✔      | ✔      | ✔      |


## Installation

Preferably use the [z3](http://hackage.haskell.org/package/z3) package.

* Install a [Z3](https://github.com/Z3Prover/z3) *4.x* release.
  (Support for Z3 *3.x* is provided by the *0.3.2* version of these bindings.)
* Just type _cabal install z3_ if you used the standard locations for dynamic libraries (_/usr/lib_) and header files (_/usr/include_).

    * Otherwise use the _--extra-lib-dirs_ and _--extra-include-dirs_ Cabal flags when installing.

## Example

Most people use the `Z3.Monad` interface.
Here is an example script that solves the 4-queen puzzle:

```haskell
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
```

In order to run this SMT script:

```haskell
main :: IO ()
main = evalZ3 script >>= \mbSol ->
        case mbSol of
             Nothing  -> error "No solution found."
             Just sol -> putStr "Solution: " >> print sol
```

## Garbage Collection

This library automatically garbage collects all C objects created through its API.

## Concurrency

Since version `408.3`, this library implements thread-safety over the C API,
i.e. API calls are serialized by locking on their `Context` argument.
To safely compile for multi-threaded code please upgrade to `>= 408.3`.

Operations and objects in different `Context`s can safely be accessed concurrently
and are not synchronized by this library.
Therefore, if you want to achieve real concurrency,
you must use a different `Context` in each thread.
You can use the `*_translate_*` functions from Z3's API to copy objects between different `Contexts`.



{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module    : Z3.Lang.Lg2
-- Copyright : (c) Iago Abal, 2012
--             (c) David Castro, 2012
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>,
--             David Castro <david.castro.dcp@gmail.com>

{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
  -- Imports deprecated modules, but it is a depcrecated module itself.

module Z3.Lang.Lg2
  {-# DEPRECATED
        "The Z3.Lang interface will be moved to a dedicated package."
        #-}
  ( declareLg2 )
  where

import Z3.Lang.Prelude

-- | Ceiling logarithm base two.
-- Axiomatization of the /lg2/ function.
-- Most likely Z3 is going to diverge if you use /lg2/ to specify a satisfiable
-- problem since it cannot construct a recursive definition for /lg2/, but it
-- should work fine for unsatisfiable instances.
declareLg2 :: IsInt a => Z3 (Expr a -> Expr a)
declareLg2 = do
  lg2::Expr a -> Expr a <- fun1
  -- invariants
  assert $ forall $ \x -> (x >* 0 ==> lg2 x >=* 0) `instanceWhen` [Pat $ lg2 x]
  assert $ forall $ \x -> (x >* 0 ==> lg2 x <* x)  `instanceWhen` [Pat $ lg2 x]
  assert $ forall $ \x -> 
              x >* 0 ==> (lg2 (x+1) ==* lg2 x ||*  lg2 (x+1) ==* 1 + lg2 x)
  -- base cases
  assert $ lg2 1 ==* 0
  assert $ lg2 2 ==* 1
  -- recursive definition
  assert $ forall (\x -> lg2 (2*x) ==* 1 + lg2 x)
  assert $ forall (\x -> lg2 (2*x+1) ==* lg2 (x+1))
  -- and that's it!
  return lg2


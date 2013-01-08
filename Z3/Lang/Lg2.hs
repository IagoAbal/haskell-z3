
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module    : Z3.Lang.Lg2
-- Copyright : (c) Iago Abal, 2012
--             (c) David Castro, 2012
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>,
--             David Castro <david.castro.dcp@gmail.com>

module Z3.Lang.Lg2
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
  assert $ forallP (\x -> x >* 0 ==> lg2 x >=* 0)
                   (\x -> Pat $ lg2 x)
  assert $ forallP (\x -> x >* 0 ==> lg2 x <* x)
                   (\x -> Pat $ lg2 x)
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


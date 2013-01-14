
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module    : Z3.Lang.Pow
-- Copyright : (c) Iago Abal, 2012
--             (c) David Castro, 2012
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>,
--             David Castro <david.castro.dcp@gmail.com>

-- TODO generalize for x^y
module Z3.Lang.Pow2
  ( declarePow2 )
  where

import Z3.Lang.Prelude

-- | Raise to the power of 2.
-- Axiomatization of the /2^n/ function.
-- Most likely Z3 is going to diverge if you use /2^n/ to specify a satisfiable
-- problem, since it cannot construct a recursive definition for /2^n/, but it
-- should work fine for unsatisfiable instances.
declarePow2 :: IsInt a => Z3 (Expr a -> Expr a)
declarePow2 = do
  pow2::Expr a -> Expr a <- fun1
  -- invariants
  assert $ forall $ \x -> (x <* 0 ==> pow2 x ==* 0)
                          `instanceWhen` [Pat $ pow2 x]
  assert $ forall $ \x -> (x >=* 0 ==> pow2 x >* 0)
                          `instanceWhen` [Pat $ pow2 x]
  assert $ forall $ \x -> (x >=* 0 ==> pow2 x >* x)
                          `instanceWhen` [Pat $ pow2 x]
  -- base cases
  assert $ pow2 0 ==* 1
  assert $ pow2 1 ==* 2
  -- recursive definition
  assert $ forall $ \x -> (x >* 1 ==> pow2 x ==* 2 * pow2 (x-1))
                          `instanceWhen` [Pat $ pow2 x]
  -- and that's it!
  return pow2


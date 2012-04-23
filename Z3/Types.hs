{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- |
-- Module    : Z3.Types
-- Copyright : (c) Iago Abal, 2011 
--             (c) David Castro, 2011
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>, 
--             David Castro <david.castro.dcp@gmail.com>


module Z3.Types (
    
    -- * Haskell Z3 Types
      TY(..)
    , Sort(..)
    , Z3Type(..)

    -- * Haskell Z3 Numerals
    , Z3Num
    , Z3Int
    , Z3Real

    ) where

------------------------------------------------------------------------
-- * Haskell Z3 Types 

-- | Type parameter. Used instead of 'undefined'
-- 
-- Example: TY :: (TY Integer) instead of undefined :: Integer
--
data TY a = TY

-- | Z3 Sort.
--
data Sort = SBool
          | SInt
          | SReal

-- | Typeclass for Haskell Z3 types, used in Z3 expressions.
--
class Z3Type a where
    sortZ3 :: TY a -> Sort

instance Z3Type Bool where
    sortZ3 _ = SBool

instance Z3Type Integer where
    sortZ3 _ = SInt

instance Z3Type Rational where
    sortZ3 _ = SReal

------------------------------------------------------------------------
-- * Haskell Z3 Numerals

-- | Typeclass for Haskell Z3 numbers.
--
class (Z3Type a, Num a) => Z3Num a where
instance Z3Num Integer where
instance Z3Num Rational where

-- | Typeclass for Haskell Z3 numbers of 'int' sort in Z3.
--
class (Z3Num a, Integral a) => Z3Int a where
instance Z3Int Integer where


-- | Typeclass for Haskell Z3 numbers of 'real' sort in Z3.
--
class (Z3Num a, Real a) => Z3Real a where
instance Z3Real Rational where

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}


-- |
-- Module    : Z3.Types
-- Copyright : (c) Iago Abal, 2012
--             (c) David Castro, 2012
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>,
--             David Castro <david.castro.dcp@gmail.com>


module Z3.Types (
    
    -- * Types
      TypeZ3
    , IsTy
    , IsScalar(..)

    -- ** Numeric types
    , IsNum
    , IsInt
    , IsReal

    ) where

import Z3.Base ( Z3Type, Z3Scalar, Z3Num )

import Data.Typeable ( Typeable )

----------------------------------------------------------------------
-- Types 

-- | Maps a type to the underlying Z3 type.
--
type family TypeZ3 a

type instance TypeZ3 Bool = Bool
type instance TypeZ3 Integer = Integer
type instance TypeZ3 Rational = Rational

-- | Types for expressions.
--
class (Typeable a, Z3Type (TypeZ3 a)) => IsTy a where

instance IsTy Bool where
instance IsTy Integer where
instance IsTy Rational where

-- | Scalar types.
--
class (Eq a, Show a, IsTy a, Z3Scalar(TypeZ3 a)) => IsScalar a where
  fromZ3Type :: TypeZ3 a -> a
  toZ3Type   :: a -> TypeZ3 a

instance IsScalar Bool where
  fromZ3Type = id
  toZ3Type = id

instance IsScalar Integer where
  fromZ3Type = id
  toZ3Type = id

instance IsScalar Rational where
  fromZ3Type = id
  toZ3Type = id

------------------------------------------------------------
-- Numeric types
--
-- Future Work: We would like to instance 'IsInt' with 'Int32' to provide
-- support for reasoning about 32-bit integer arithmetic with overflow.
-- It would be also interesting (but perhaps more tricky) to support
-- floating point arithmetic by creating an instance of 'IsReal' for
-- 'Double'.
--

-- | Numeric types.
--
class (IsScalar a, Num a, Z3Num (TypeZ3 a)) => IsNum a where
instance IsNum Integer where
instance IsNum Rational where

-- | Typeclass for Haskell Z3 numbers of /int/ sort in Z3.
--
class (IsNum a, Integral a, TypeZ3 a ~ Integer) => IsInt a where
instance IsInt Integer where

-- | Typeclass for Haskell Z3 numbers of /real/ sort in Z3.
--
class (IsNum a, Fractional a, Real a, TypeZ3 a ~ Rational) => IsReal a where
instance IsReal Rational where

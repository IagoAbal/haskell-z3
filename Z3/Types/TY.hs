
{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module    : Z3.Types.TY
-- Copyright : (c) Iago Abal, 2012
--             (c) David Castro, 2012
-- License   : BSD3
-- Maintainer: Iago Abal <iago.abal@gmail.com>, 
--             David Castro <david.castro.dcp@gmail.com>

module Z3.Types.TY where


import Data.Data ( Data, Typeable )


-- | An alternative to 'undefined' to fake type parameters.
-- 
-- Example: @TY :: TY Integer@ instead of @undefined :: Integer@
--
data TY a = TY
    deriving (Data,Typeable)

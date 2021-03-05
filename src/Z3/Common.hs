-- |
-- Module    : Z3.Common
-- Copyright : (c) Iago Abal, 2013-2015
--             (c) David Castro, 2013
-- License   : BSD3
-- Maintainer: Iago Abal <mail@iagoabal.eu>,
--             David Castro <david.castro.dcp@gmail.com>
--
-- Common utils for other modules.
--
--

module Z3.Common where

-- | Wraps a monadic value in a Maybe as indicated by a boolean flag
returnValueToMaybe :: Monad m => Bool -> m a -> m (Maybe a)
returnValueToMaybe success m = if success then do
                                 val <- m
                                 return $ Just val
                               else
                                 return Nothing


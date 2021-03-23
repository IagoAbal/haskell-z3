-- |
-- Module    : Z3.Lock
-- Copyright  : (c) 2010-2011 Bas van Dijk & Roel van Dijk
-- License   : BSD3
-- Maintainer: Iago Abal <mail@iagoabal.eu>,
--             David Castro <david.castro.dcp@gmail.com>
--
-- A minimal implementation of a re-entrant lock, adopted from https://github.com/basvandijk/concurrent-extra
--
--

module Z3.Lock where

import Control.Concurrent( MVar, takeMVar, newMVar, tryPutMVar, readMVar )
import Control.Monad( when )

-- | A lock is in one of two states: \"locked\" or \"unlocked\".
newtype Lock = Lock {un :: MVar ()} deriving Eq

-- | Create a lock in the \"unlocked\" state.
new :: IO Lock
new = Lock <$> newMVar ()

{-|
Acquires the 'Lock'. Blocks if another thread has acquired the 'Lock'.

@acquire@ behaves as follows:

* When the state is \"unlocked\" @acquire@ changes the state to \"locked\".

* When the state is \"locked\" @acquire@ /blocks/ until a call to 'release' in
another thread wakes the calling thread. Upon awakening it will change the state
to \"locked\".

There are two further important properties of @acquire@:

* @acquire@ is single-wakeup. That is, if there are multiple threads blocked on
@acquire@ and the lock is released, only one thread will be woken up. The
runtime guarantees that the woken thread completes its @acquire@ operation.

* When multiple threads are blocked on @acquire@, they are woken up in FIFO
order. This is useful for providing fairness properties of abstractions built
using locks. (Note that this differs from the Python implementation where the
wake-up order is undefined.)
-}
acquire :: Lock -> IO ()
acquire = takeMVar . un

{-|
@release@ changes the state to \"unlocked\" and returns immediately.

Note that it is an error to release a lock in the \"unlocked\" state!

If there are any threads blocked on 'acquire' the thread that first called
@acquire@ will be woken up.
-}
release :: Lock -> IO ()
release (Lock mv) = do
  b <- tryPutMVar mv ()
  when (not b) $ error "Z3.Lock.release: Can't release unlocked Lock!"

{-|
* When the state is \"locked\", @wait@ /blocks/ until a call to 'release' in
another thread changes it to \"unlocked\".

* @wait@ is multiple-wakeup, so when multiple waiters are blocked on a @Lock@,
  all of them are woken up at the same time.

* When the state is \"unlocked\" @wait@ returns immediately.

@wait@ does not alter the state of the lock.
-}
wait :: Lock -> IO ()
wait (Lock mv) = readMVar mv

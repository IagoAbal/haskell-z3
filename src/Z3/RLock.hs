{-# LANGUAGE BangPatterns #-}

-- |
-- Module    : Z3.RLock
-- Copyright  : (c) 2010-2011 Bas van Dijk & Roel van Dijk
-- License   : BSD3
-- Maintainer: Iago Abal <mail@iagoabal.eu>,
--             David Castro <david.castro.dcp@gmail.com>
--
-- A minimal implementation of a re-entrant lock, adapted from https://github.com/basvandijk/concurrent-extra
--
--

module Z3.RLock where

import qualified Z3.Lock as Lock

import Z3.Lock(Lock)

import Control.Concurrent( ThreadId, myThreadId, MVar, putMVar, takeMVar, newMVar )
import Control.Exception( mask_, bracket_ )
import Control.Applicative( liftA2 )

{-| A reentrant lock is in one of two states: \"locked\" or \"unlocked\". When
the lock is in the \"locked\" state it has two additional properties:

* Its /owner/: the thread that acquired the lock.

* Its /acquired count/: how many times its owner acquired the lock.
-}
newtype RLock = RLock {un :: MVar (State, Lock)}
    deriving Eq
{-| The state of an 'RLock'.

* 'Nothing' indicates an \"unlocked\" state.

* @'Just' (tid, n)@ indicates a \"locked\" state where the thread identified by
@tid@ acquired the lock @n@ times.
-}
type State = Maybe (ThreadId, Integer)

-- | Create a reentrant lock in the \"unlocked\" state.
new :: IO RLock
new = do lock <- Lock.new
         RLock <$> newMVar (Nothing, lock)

{-|
Acquires the 'RLock'. Blocks if another thread has acquired the 'RLock'.
@acquire@ behaves as follows:

* When the state is \"unlocked\", @acquire@ changes the state to \"locked\"
with the current thread as owner and an acquired count of 1.

* When the state is \"locked\" and the current thread owns the lock @acquire@
only increments the acquired count.

* When the state is \"locked\" and the current thread does not own the lock
@acquire@ /blocks/ until the owner releases the lock. If the thread that called
@acquire@ is woken upon release of the lock it will take ownership and change
the state to \"locked\" with an acquired count of 1.

There are two further important properties of @acquire@:

* @acquire@ is single-wakeup. That is, if there are multiple threads blocked on
@acquire@, and the lock is released, only one thread will be woken up. The
runtime guarantees that the woken thread completes its @acquire@ operation.

* When multiple threads are blocked on @acquire@ they are woken up in FIFO
order. This is useful for providing fairness properties of abstractions built
using locks. (Note that this differs from the Python implementation where the
wake-up order is undefined.)
-}
acquire :: RLock -> IO ()
acquire (RLock mv) = do
  myTID <- myThreadId
  mask_ $ let acq = do t@(mb, lock) <- takeMVar mv
                       case mb of
                         Nothing          -> do Lock.acquire lock
                                                putMVar mv (Just (myTID, 1), lock)
                         Just (tid, n)
                           | myTID == tid -> let !sn = succ n
                                             in putMVar mv (Just (tid, sn), lock)
                           | otherwise    -> do putMVar mv t
                                                Lock.wait lock
                                                acq
          in acq

{-| @release@ decrements the acquired count. When a lock is released with an
acquired count of 1 its state is changed to \"unlocked\".

Note that it is both an error to release a lock in the \"unlocked\" state and to
release a lock that is not owned by the current thread.

If there are any threads blocked on 'acquire' the thread that first called
@acquire@ will be woken up.
-}
release :: RLock -> IO ()
release (RLock mv) = do
  myTID <- myThreadId
  mask_ $ do
    t@(mb, lock) <- takeMVar mv
    let err msg = do putMVar mv t
                     error $ "Z3.RLock.release: " ++ msg
    case mb of
      Nothing -> err "Can't release an unacquired RLock!"
      Just (tid, n)
        | myTID == tid -> if n == 1
                          then do Lock.release lock
                                  putMVar mv (Nothing, lock)
                          else let !pn = pred n
                               in putMVar mv (Just (tid, pn), lock)
        | otherwise -> err "Calling thread does not own the RLock!"

{-| A convenience function which first acquires the lock and then
performs the computation. When the computation terminates, whether
normally or by raising an exception, the lock is released.

Note that: @with = 'liftA2' 'bracket_' 'acquire' 'release'@.
-}
with :: RLock -> IO a -> IO a
with = liftA2 bracket_ acquire release

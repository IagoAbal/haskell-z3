{-# LANGUAGE ParallelListComp, TupleSections #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.List
import qualified Data.Traversable as T

import System.Environment

import Z3.Lang


nqueens :: Int -> Z3 [Expr Integer]
nqueens n = do
  -- the ith-queen is in the ith-row.
  -- qi is the column of the ith-queen
  qs <- replicateM n var
  mapM_ assert [ 1 <=* q &&* q <=* literal (toInteger n) | q <- qs ]
  -- different columns
  assert $ distinct qs
  -- avoid diagonal attacks
  mapM_ assert $ concat [ [ not_ $ diagonal d c c'
                          | d <- [1..]
                          | c' <- cs'
                          ]
                        | c <- init qs
                        | cs' <- tail $ tails qs
                        ]
  return qs

diagonal :: Integer -> Expr Integer -> Expr Integer -> Expr Bool
diagonal d c c' = abs(c'-c) ==* fromInteger d

nqueensAll :: Int -> IO [[Integer]]
nqueensAll n = go []
  where go ss = do
          let script = do
                qs <- nqueens n
                notTheseSolutions qs ss
                checkModel $ evalT qs
          mbSol <- evalZ3 script
          case mbSol of
               Just s  -> (s:) <$> go (s:ss)
               Nothing -> return []

notTheseSolutions :: [Expr Integer] -> [[Integer]] -> Z3 ()
notTheseSolutions qs ss = mapM_ assert
  [ not_ $ and_ $ zipWith (\q c -> q ==* literal c) qs s | s <- ss ]

main = do
  (argN:_) <- getArgs
  nqueensAll (read argN) >>= print

{-# LANGUAGE ParallelListComp #-}

module Main where

import Control.Monad ( replicateM )
import Data.List ( tails )
import qualified Data.Traversable as T

import System.Environment

import Z3.Lang


script :: Int -> Z3 (Maybe [Integer])
script n = do
  -- the ith-queen is in the ith-row.
  -- qi is the column of the ith-queen
  qs <- replicateM n var
  mapM_ assert [ 1 <=* q &&* q <=* fromIntegral n | q <- qs ]
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
  -- check and get solution
  checkModel $ evalT qs

diagonal :: Integer -> Expr Integer -> Expr Integer -> Expr Bool
diagonal d c c' = abs(c'-c) ==* fromInteger d

main = do
  (argN:_) <- getArgs
  evalZ3 (script $ read argN) >>= print

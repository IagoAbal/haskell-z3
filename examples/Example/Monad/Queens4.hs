-- | The 4-queens puzzle.
module Example.Monad.Queens4
  ( run )
  where

import Control.Applicative
import Control.Monad ( join )
import Data.Maybe
import qualified Data.Traversable as T

import Z3.Monad

run :: IO ()
run = evalZ3With Nothing opts script >>= \mbSol ->
        case mbSol of
             Nothing  -> error "No solution found."
             Just sol -> putStr "Solution: " >> print sol
  where opts = opt "MODEL" True +? opt "MODEL_COMPLETION" True

script :: Z3 (Maybe [Integer])
script = do
  q1 <- mkFreshIntVar "q1"
  q2 <- mkFreshIntVar "q2"
  q3 <- mkFreshIntVar "q3"
  q4 <- mkFreshIntVar "q4"
  _1 <- mkInteger 1
  _4 <- mkInteger 4
  -- the ith-queen is in the ith-row.
  -- qi is the column of the ith-queen
  assert =<< mkAnd =<< T.sequence
    [ mkLe _1 q1, mkLe q1 _4  -- 1 <= q1 <= 4
    , mkLe _1 q2, mkLe q2 _4
    , mkLe _1 q3, mkLe q3 _4
    , mkLe _1 q4, mkLe q4 _4
    ]
  -- different columns
  assert =<< mkDistinct [q1,q2,q3,q4]
  -- avoid diagonal attacks
  assert =<< mkNot =<< mkOr =<< T.sequence
    [ diagonal 1 q1 q2  -- diagonal line of attack between q1 and q2
    , diagonal 2 q1 q3
    , diagonal 3 q1 q4
    , diagonal 1 q2 q3
    , diagonal 2 q2 q4
    , diagonal 1 q3 q4
    ]
  -- check and get solution
  fmap snd $ withModel $ \m ->
    catMaybes <$> mapM (evalInt m) [q1,q2,q3,q4]
  where mkAbs x = do
          _0 <- mkInteger 0
          join $ mkIte <$> mkLe _0 x <*> pure x <*> mkUnaryMinus x
        diagonal d c c' =
          join $ mkEq <$> (mkAbs =<< mkSub [c',c]) <*> (mkInteger d)

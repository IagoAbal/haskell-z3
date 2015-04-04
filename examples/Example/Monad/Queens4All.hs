-- | All the solutions of the 4-queens puzzle.
module Example.Monad.Queens4All
  ( run )
  where

import Control.Applicative
import Control.Monad ( join )
import Data.Maybe
import qualified Data.Traversable as T

import Z3.Monad

run :: IO ()
run = do
  sols <- evalZ3With Nothing opts script
  putStrLn "Solutions: "
  mapM_ print sols
  where opts = opt "MODEL" True +? opt "MODEL_COMPLETION" True


type Solution = [Integer]

getSolutions :: AST -> AST -> AST -> AST -> Z3 [Solution]
getSolutions q1 q2 q3 q4 = go []
  where go acc = do
          mb_sol <- getSolution
          case mb_sol of
               Nothing  -> return acc
               Just sol -> do restrictSolution sol
                              go (sol:acc)
        restrictSolution :: Solution -> Z3 ()
        restrictSolution [c1,c2,c3,c4] =
          assert =<< mkNot =<< mkOr =<< T.sequence
            [ mkEq q1 =<< mkIntNum c1
            , mkEq q2 =<< mkIntNum c2
            , mkEq q3 =<< mkIntNum c3
            , mkEq q4 =<< mkIntNum c4
            ]
        restrictSolution _____________ = error "invalid argument"
        getSolution :: Z3 (Maybe Solution)
        getSolution = fmap snd $ withModel $ \m ->
          catMaybes <$> mapM (evalInt m) [q1,q2,q3,q4]

script :: Z3 [Solution]
script = do
  q1 <- mkFreshIntVar "q1"
  q2 <- mkFreshIntVar "q2"
  q3 <- mkFreshIntVar "q3"
  q4 <- mkFreshIntVar "q4"
  _1 <- mkIntNum (1::Integer)
  _4 <- mkIntNum (4::Integer)
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
  getSolutions q1 q2 q3 q4
  where mkAbs :: AST -> Z3 AST
        mkAbs x = do
          _0 <- mkIntNum (0::Integer)
          join $ mkIte <$> mkLe _0 x <*> pure x <*> mkUnaryMinus x
        diagonal :: Integer -> AST -> AST -> Z3 AST
        diagonal d c c' =
          join $ mkEq <$> (mkAbs =<< mkSub [c',c]) <*> (mkIntNum d)

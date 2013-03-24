module Main where

import Control.Applicative
import Control.Monad ( join )
import Control.Monad.Trans
import Data.Maybe
import qualified Data.Traversable as T

import Z3.Monad


script :: Z3 (Maybe [Integer])
script = do
  intSort <- mkIntSort
  q1 <- flip mkConst intSort =<< mkStringSymbol "q1"
  q2 <- flip mkConst intSort =<< mkStringSymbol "q2"
  q3 <- flip mkConst intSort =<< mkStringSymbol "q3"
  q4 <- flip mkConst intSort =<< mkStringSymbol "q4"
  _1 <- mkInt 1
  _4 <- mkInt 4
  -- the ith-queen is in the ith-row.
  -- qi is the column of the ith-queen
  assertCnstr =<< mkAnd =<< T.sequence [mkLe _1 q1, mkLe q1 _4]
  assertCnstr =<< mkAnd =<< T.sequence [mkLe _1 q2, mkLe q2 _4]
  assertCnstr =<< mkAnd =<< T.sequence [mkLe _1 q3, mkLe q3 _4]
  assertCnstr =<< mkAnd =<< T.sequence [mkLe _1 q4, mkLe q4 _4]
  -- different columns
  assertCnstr =<< mkDistinct [q1,q2,q3,q4]
  -- avoid diagonal attacks
  assertCnstr =<< mkNot =<< diagonal 1 q1 q2
  assertCnstr =<< mkNot =<< diagonal 2 q1 q3
  assertCnstr =<< mkNot =<< diagonal 3 q1 q4
  assertCnstr =<< mkNot =<< diagonal 1 q2 q3
  assertCnstr =<< mkNot =<< diagonal 2 q2 q4
  assertCnstr =<< mkNot =<< diagonal 1 q3 q4
  -- check and get solution
  withModel $ \m -> do
    mb_cs <- evalT m [q1,q2,q3,q4]
    mapM getInt $ fromJust mb_cs
  where mkAbs :: AST -> Z3 AST
        mkAbs x = do
          _0 <- mkInt 0
          join $ mkIte <$> mkLe _0 x <*> pure x <*> mkUnaryMinus x
        diagonal d c c' =
          join $ mkEq <$> (mkAbs =<< mkSub [c',c]) <*> (mkInt d)

main = evalZ3With Nothing opts script >>= print
  where opts = opt "MODEL" True +? opt "MODEL_COMPLETION" True

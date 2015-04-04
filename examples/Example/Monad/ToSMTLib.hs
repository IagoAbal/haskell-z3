-- | Generate SMTLib.
module Example.Monad.ToSMTLib
  ( run )
  where

import Control.Applicative
import Control.Monad ( join )
import Control.Monad.Trans
import Data.Maybe
import qualified Data.Traversable as T

import Z3.Monad

run :: IO ()
run = evalZ3 script >>= putStrLn

script :: Z3 String
script = do
  intSort <- mkIntSort
  q1 <- flip mkConst intSort =<< mkStringSymbol "q1"
  q2 <- flip mkConst intSort =<< mkStringSymbol "q2"
  q3 <- flip mkConst intSort =<< mkStringSymbol "q3"
  q4 <- flip mkConst intSort =<< mkStringSymbol "q4"
  _1 <- mkIntNum (1::Integer)
  _4 <- mkIntNum (4::Integer)
  -- the ith-queen is in the ith-row.
  -- qi is the column of the ith-queen
  as1 <- mkAnd =<< T.sequence [mkLe _1 q1, mkLe q1 _4]
  as2 <- mkAnd =<< T.sequence [mkLe _1 q2, mkLe q2 _4]
  as3 <- mkAnd =<< T.sequence [mkLe _1 q3, mkLe q3 _4]
  as4 <- mkAnd =<< T.sequence [mkLe _1 q4, mkLe q4 _4]
  -- different columns
  as5 <- mkDistinct [q1,q2,q3,q4]
  -- avoid diagonal attacks
  as6 <- mkNot =<< diagonal 1 q1 q2
  as7 <- mkNot =<< diagonal 2 q1 q3
  as8 <- mkNot =<< diagonal 3 q1 q4
  as9 <- mkNot =<< diagonal 1 q2 q3
  as10 <- mkNot =<< diagonal 2 q2 q4
  as11 <- mkNot =<< diagonal 1 q3 q4
  -- SMTLib script
  _true <- mkTrue
  benchmarkToSMTLibString "queens" "QF_LIA" "unknown" ""
                          [as1,as2,as3,as4,as5,as6,as7,as8,as9,as10,as11]
                          _true
  where mkAbs :: AST -> Z3 AST
        mkAbs x = do
          _0 <- mkIntNum (0::Integer)
          join $ mkIte <$> mkLe _0 x <*> pure x <*> mkUnaryMinus x
        diagonal d c c' =
          join $ mkEq <$> (mkAbs =<< mkSub [c',c]) <*> (mkIntNum d)

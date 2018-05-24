module Example.Monad.Quantifiers
  ( run )
  where

import Control.Monad
import Control.Monad.IO.Class (liftIO)

import Z3.Monad

run = evalZ3 $ do
  a <- mkFreshIntVar "a"
  b <- mkFreshIntVar "b"
  c <- mkFreshIntVar "c"
  d <- mkFreshIntVar "d"

  a' <- toApp a
  b' <- toApp b
  c' <- toApp c
  d' <- toApp d

  fs <- sequence [ mkEq a =<< mkAdd [ b, c ]
                 , mkExistsConst [] [a'] =<< mkEq a =<< mkAdd [ b, c ]
                 , mkForallConst [] [a', d'] =<< mkExistsConst [] [ b', c' ] =<< join (liftM2 mkEq (mkAdd [ a, b ]) (mkMul [ c, d ]))
                 , mkAdd [ a, b, c, d ] ]

  forM_ fs $ \a -> do
    k <- getAstKind a
    case k of
      Z3_QUANTIFIER_AST -> do
        t  <- isQuantifierForall a
        vs <- getQuantifierBoundVars a
        b  <- getQuantifierBody a
        f  <- substituteVars a vs -- this step only replaces debruijn encoding of the bound variables
        liftIO . putStrLn . ((if t then "universal: " else "existential: ") ++) =<< astToString f
      _ -> liftIO . putStrLn =<< astToString a

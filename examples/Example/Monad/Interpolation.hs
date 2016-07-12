-- | Compute interpolant for an unsatisfiable conjunction of formulas
module Example.Monad.Interpolation
  ( run )
  where

import Control.Monad
import Control.Monad.Trans ( liftIO )

import Z3.Monad

run :: IO ()
run = do
    env <- newItpEnv Nothing stdOpts
    evalZ3WithEnv z3 env

z3 = do
    a <- mkFreshBoolVar "a"
    b <- mkFreshBoolVar "b"

    na <- mkNot a
    nb <- mkNot b

    f1 <- mkIff a b
    f2 <- mkIff a nb

    g1 <- mkInterpolant f1
    g2 <- mkInterpolant f2
    g3 <- mkInterpolant =<< mkTrue

    params <- mkParams
    res <- flip computeInterpolant params =<< mkAnd [g1, g2, g3]

    case res of
        Just (Right itps) -> mapM_ (liftIO . putStrLn <=< astToString) itps
        otherwise         -> error "could not compute interpolants"

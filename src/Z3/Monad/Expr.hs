
module Z3.Monad.Expr where

import           Z3.Monad ( AST, MonadZ3 )
import qualified Z3.Monad as Z3

import Control.Monad ( (=<<), join )

import Data.Ratio ( numerator, denominator )


---------------------------------------------------------------------
-- * Booleans

true, false :: MonadZ3 z3 => z3 AST
true  = Z3.mkTrue
false = Z3.mkFalse

not_ :: MonadZ3 z3 => z3 AST -> z3 AST
not_ za = Z3.mkNot =<< za

infixr 3  &&*, ||*, `xor`
infixr 2  `implies`, `iff`, ==>, <=>

(&&*), (||*), xor, implies, iff, (==>), (<=>)
  :: MonadZ3 z3 => z3 AST -> z3 AST -> z3 AST

za &&* zb = do
  a <- za
  b <- zb
  Z3.mkAnd [a, b]

za ||* zb = do
  a <- za
  b <- zb
  Z3.mkOr [a, b]

za `xor` zb = join $ Z3.mkXor <$> za <*> zb

za `implies` zb = join $ Z3.mkImplies <$> za <*> zb

za `iff` zb = join $ Z3.mkIff <$> za <*> zb

(==>) = implies
(<=>) = iff


---------------------------------------------------------------------
-- * Comparisons

infix  4  ==*, /=*, <*, <=*, >=*, >*

(==*), (/=*), (<*), (<=*), (>=*), (>*)
  :: MonadZ3 z3 => z3 AST -> z3 AST -> z3 AST

za ==* zb = join $ Z3.mkEq <$> za <*> zb

za /=* zb = do
  a <- za
  b <- zb
  Z3.mkDistinct [a, b]

za <* zb = join $ Z3.mkLt <$> za <*> zb

za <=* zb = join $ Z3.mkLe <$> za <*> zb

za >=* zb = join $ Z3.mkGe <$> za <*> zb

za >* zb = join $ Z3.mkGt <$> za <*> zb


---------------------------------------------------------------------
-- * Comparisons

ite :: MonadZ3 z3 => z3 AST -> z3 AST -> z3 AST -> z3 AST
ite zc za zb = join $ Z3.mkIte <$> zc <*> za <*> zb


---------------------------------------------------------------------
-- * Integers and Reals

-- Mainly for integers but +/-/* also work for reals
instance MonadZ3 z3 => Num (z3 AST) where
  za + zb = do
    a <- za
    b <- zb
    Z3.mkAdd [a, b]
  za - zb = do
    a <- za
    b <- zb
    Z3.mkSub [a, b]
  za * zb = do
    a <- za
    b <- zb
    Z3.mkMul [a, b]
  negate za = Z3.mkUnaryMinus =<< za
  -- abs may only work for integers?
  abs za = ite (za >=* 0) za (-za)
  -- signum may only work for integers?
  signum za =
    ite (za >* 0)
        1
    (ite (za ==* 0)
        0
        (-1))
  fromInteger = Z3.mkInteger

-- For reals
instance MonadZ3 z3 => Fractional (z3 AST) where
  za / zb = join $ Z3.mkDiv <$> za <*> zb
  fromRational r = Z3.mkNumeral r_str =<< Z3.mkRealSort
    where r_num = toInteger $ numerator r
          r_den = toInteger $ denominator r
          r_str = show r_num ++ " / " ++ show r_den

-- TODO: Data.Bits for bit-vectors
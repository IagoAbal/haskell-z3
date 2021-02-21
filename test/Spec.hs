
import Test.Hspec

import qualified Z3.Base.Spec
import qualified Z3.Monad.Spec
import qualified Z3.Regression
import qualified Z3.Tutorial

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Z3.Base" Z3.Base.Spec.spec
  describe "Z3.Monad" Z3.Monad.Spec.spec
  describe "Z3.Regression" Z3.Regression.spec
  describe "Z3.Tutorial" Z3.Tutorial.spec

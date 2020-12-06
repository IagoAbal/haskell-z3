
import Test.Hspec

import qualified Z3.Base.Spec
import qualified Z3.Regression

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Z3.Base" Z3.Base.Spec.spec
  describe "Z3.Regression" Z3.Regression.spec

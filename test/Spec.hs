
import Test.Hspec

import qualified Z3.Base.Spec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Z3.Base" Z3.Base.Spec.spec

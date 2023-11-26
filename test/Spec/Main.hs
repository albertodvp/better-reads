module Main (main) where

import ServiceSpec qualified
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "Service" ServiceSpec.specs

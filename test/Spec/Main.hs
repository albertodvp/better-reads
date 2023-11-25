module Main (main) where

import Internal.SanitizeSpec qualified
import ServiceSpec qualified
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "Service" ServiceSpec.specs
  context "Internal.Sanitize" Internal.SanitizeSpec.specs

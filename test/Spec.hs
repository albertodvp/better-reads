module Main (main) where

import DomainSpec qualified
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  context "Domain" DomainSpec.spec

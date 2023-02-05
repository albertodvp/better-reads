module Main (main) where

import Test.Hspec

import qualified DomainSpec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    context "Domain" DomainSpec.spec

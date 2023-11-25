module Internal.SanitizeSpec (specs) where

import Test.Hspec (Spec, describe, it, shouldBe)

specs :: Spec
specs = sequence_ [specUnnormalizeCSVHeader, specNormalizeCSVHeader]

specUnnormalizeCSVHeader :: Spec
specUnnormalizeCSVHeader = describe "normalizeCSVHeader" $ do
  it "" $ do
    True `shouldBe` False

specNormalizeCSVHeader :: Spec
specNormalizeCSVHeader = describe "unnormalizeCSVHeader" $ do
  it "" $ do
    True `shouldBe` False

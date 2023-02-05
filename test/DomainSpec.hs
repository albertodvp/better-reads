-- |
module DomainSpec(spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Instances

import qualified Data.ByteString.Lazy      as B
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           Domain                    (Book (..), encodeBooks, parseBooks)

-- TODO: how can I derive this automagically?
instance Arbitrary Book where
  arbitrary = do
    title <- arbitrary
    author <- arbitrary
    isbn13 <- arbitrary
    return $ Book {..}

spec :: Spec
spec = do
  describe "parseBooks" $ do
    it "parses an empty vector if the header is not present" $ do
      let (Right (_, books)) = parseBooks "What We Owe the Future,William MacAskill,9781541618626\n"
      books `shouldBe` V.fromList []

    it "parses correctly one book" $ do
      let (Right (_, books)) = parseBooks "Title,Author,ISBN13\nWhat We Owe the Future,William MacAskill,9781541618626\n"
      books `shouldBe` V.fromList [
        Book "What We Owe the Future" "William MacAskill" "9781541618626"
        ]

    it "parses correctly one book (mixed fields)" $ do
      let (Right (_, books)) = parseBooks "Author,ISBN13,Title\nWilliam MacAskill,9781541618626,What We Owe the Future\n"
      books `shouldBe` V.fromList [
        Book "What We Owe the Future" "William MacAskill" "9781541618626"
        ]

    it "parses correctly one book (extra fields)" $ do
      let (Right (_, books)) = parseBooks "DontCare,Author,ISBN13,Title\n42,William MacAskill,9781541618626,What We Owe the Future\n"
      books `shouldBe` V.fromList [
        Book "What We Owe the Future" "William MacAskill" "9781541618626"
        ]


    it "parses correctly two books" $ do
      let (Right (_, books)) = parseBooks "Title,Author,ISBN13\nWhat We Owe the Future,William MacAskill,9781541618626\nData Mesh: Delivering Data-Driven Value at Scale,Zhamak Dehghani,9781492092391"
      books `shouldBe` V.fromList [ Book "What We Owe the Future" "William MacAskill" "9781541618626"
                                  , Book "Data Mesh: Delivering Data-Driven Value at Scale" "Zhamak Dehghani" "9781492092391"
                                  ]
  describe "encodeBooks" $ do
    it "encodes a single book" $ do
      let
        encodedBooks = encodeBooks [Book "What We Owe the Future" "William MacAskill" "9781541618626"]
        expected = "Title,Author,ISBN13\nWhat We Owe the Future,William MacAskill,9781541618626\n"
      encodedBooks `shouldBe`expected

    it "encodes two books" $ do
      let
        encodedBooks = encodeBooks [ Book "What We Owe the Future" "William MacAskill" "9781541618626"
                                   , Book "Data Mesh: Delivering Data-Driven Value at Scale" "Zhamak Dehghani" "9781492092391"
                                   ]
        expected = "Title,Author,ISBN13\nWhat We Owe the Future,William MacAskill,9781541618626\nData Mesh: Delivering Data-Driven Value at Scale,Zhamak Dehghani,9781492092391\n"
      encodedBooks `shouldBe`expected

  describe "property based encode/parse" $ do
    prop "parse . encode = id" $
      \books -> let Right (_, encodedParsedBooks) = parseBooks (encodeBooks books) in encodedParsedBooks `shouldBe` books


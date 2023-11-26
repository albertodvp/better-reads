{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ServiceSpec (specs) where

import AppTypes (BooksFile (BooksFile))
import Data.Time (fromGregorian)
import Internal.Domain (Book (..))
import Service (encodeBooks, parseBooks)
import Test.Hspec (Spec, describe, it, shouldBe)

specs :: Spec
specs = sequence_ [specParseBooks, specEncodeBooks]

specParseBooks :: Spec
specParseBooks = describe "parseBooks" $ do
  it "parses an empty vector if the header is not present" $ do
    let books = parseBooks $ BooksFile "What We Owe the Future,William MacAskill,9781541618626,2023/11/08\n"
    books `shouldBe` Right []

  it "parses correctly one book" $ do
    let books = parseBooks $ BooksFile "Title,Author,ISBN13,Date Added\nWhat We Owe the Future,William MacAskill,9781541618626,2023/11/08\n"
    books `shouldBe` Right [Book "What We Owe the Future" "William MacAskill" "9781541618626" (fromGregorian 2023 11 8)]

  it "parses correctly one book (mixed fields)" $ do
    let books = parseBooks $ BooksFile "Author,ISBN13,Title,Date Added\nWilliam MacAskill,9781541618626,What We Owe the Future,2023/11/08\n"
    books
      `shouldBe` Right [Book "What We Owe the Future" "William MacAskill" "9781541618626" (fromGregorian 2023 11 8)]
  it "parses correctly one book (extra fields)" $ do
    let books = parseBooks $ BooksFile "DontCare,Author,ISBN13,Title,Date Added\n42,William MacAskill,9781541618626,What We Owe the Future,2023/11/08\n"
    books
      `shouldBe` Right [Book "What We Owe the Future" "William MacAskill" "9781541618626" (fromGregorian 2023 11 8)]

  it "parses correctly two books" $ do
    let books = parseBooks $ BooksFile "Title,Author,ISBN13,Date Added\nWhat We Owe the Future,William MacAskill,9781541618626,2023/11/08\nData Mesh: Delivering Data-Driven Value at Scale,Zhamak Dehghani,9781492092391,2022/10/01\n"
    books
      `shouldBe` Right
        [ Book "What We Owe the Future" "William MacAskill" "9781541618626" (fromGregorian 2023 11 8),
          Book "Data Mesh: Delivering Data-Driven Value at Scale" "Zhamak Dehghani" "9781492092391" (fromGregorian 2022 10 1)
        ]

specEncodeBooks :: Spec
specEncodeBooks = describe "encodeBooks" $ do
  it "encodes a single book" $ do
    let (BooksFile encodedBooks) = encodeBooks [Book "What We Owe the Future" "William MacAskill" "9781541618626" (fromGregorian 2023 11 8)]
        expected = "Title,Author,ISBN13,Date Added\nWhat We Owe the Future,William MacAskill,9781541618626,2023/11/08\n"
    encodedBooks `shouldBe` expected

  it "encodes two books" $ do
    let (BooksFile encodedBooks) =
          encodeBooks
            [ Book "What We Owe the Future" "William MacAskill" "9781541618626" (fromGregorian 2023 11 8),
              Book "Data Mesh: Delivering Data-Driven Value at Scale" "Zhamak Dehghani" "9781492092391" (fromGregorian 2022 10 1)
            ]
        expected = "Title,Author,ISBN13,Date Added\nWhat We Owe the Future,William MacAskill,9781541618626,2023/11/08\nData Mesh: Delivering Data-Driven Value at Scale,Zhamak Dehghani,9781492092391,2022/10/01\n"
    encodedBooks `shouldBe` expected

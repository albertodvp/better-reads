{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module ServiceSpec (specs) where

import AppTypes (BooksFile (BooksFile))
import Internal.Domain (Book (..))
import Service (encodeBooks, parseBooks)
import Test.Hspec (Spec, describe, it, shouldBe)

specs :: Spec
specs = sequence_ [specParseBooks, specEncodeBooks]

specParseBooks :: Spec
specParseBooks = describe "parseBooks" $ do
  it "parses an empty vector if the header is not present" $ do
    let books = parseBooks $ BooksFile "What We Owe the Future,William MacAskill,9781541618626\n"
    books `shouldBe` Right []

  it "parses correctly one book lower case" $ do
    let books = parseBooks $ BooksFile "title,author,isbn13\nWhat We Owe the Future,William MacAskill,9781541618626\n"
    books `shouldBe` Right [Book "What We Owe the Future" "William MacAskill" "9781541618626"]

  it "parses correctly one book" $ do
    let books = parseBooks $ BooksFile "Title,Author,ISBN13\nWhat We Owe the Future,William MacAskill,9781541618626\n"
    books `shouldBe` Right [Book "What We Owe the Future" "William MacAskill" "9781541618626"]

  it "parses correctly one book (mixed fields)" $ do
    let books = parseBooks $ BooksFile "Author,ISBN13,Title\nWilliam MacAskill,9781541618626,What We Owe the Future\n"
    books
      `shouldBe` Right [Book "What We Owe the Future" "William MacAskill" "9781541618626"]
  it "parses correctly one book (extra fields)" $ do
    let books = parseBooks $ BooksFile "DontCare,Author,ISBN13,Title\n42,William MacAskill,9781541618626,What We Owe the Future\n"
    books
      `shouldBe` Right [Book "What We Owe the Future" "William MacAskill" "9781541618626"]

  it "parses correctly two books" $ do
    let books = parseBooks $ BooksFile "Title,Author,ISBN13\nWhat We Owe the Future,William MacAskill,9781541618626\nData Mesh: Delivering Data-Driven Value at Scale,Zhamak Dehghani,9781492092391"
    books
      `shouldBe` Right
        [ Book "What We Owe the Future" "William MacAskill" "9781541618626",
          Book "Data Mesh: Delivering Data-Driven Value at Scale" "Zhamak Dehghani" "9781492092391"
        ]

specEncodeBooks :: Spec
specEncodeBooks = describe "encodeBooks" $ do
  it "encodes a single book" $ do
    let (BooksFile encodedBooks) = encodeBooks [Book "What We Owe the Future" "William MacAskill" "9781541618626"]
        expected = "Title,Author,ISBN13\nWhat We Owe the Future,William MacAskill,9781541618626\n"
    encodedBooks `shouldBe` expected

  it "encodes two books" $ do
    let (BooksFile encodedBooks) =
          encodeBooks
            [ Book "What We Owe the Future" "William MacAskill" "9781541618626",
              Book "Data Mesh: Delivering Data-Driven Value at Scale" "Zhamak Dehghani" "9781492092391"
            ]
        expected = "Title,Author,ISBN13\nWhat We Owe the Future,William MacAskill,9781541618626\nData Mesh: Delivering Data-Driven Value at Scale,Zhamak Dehghani,9781492092391\n"
    encodedBooks `shouldBe` expected

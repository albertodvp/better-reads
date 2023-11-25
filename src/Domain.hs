{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright: (c) 2023 Alberto Fanton
-- SPDX-License-Identifier: MIT
-- Maintainer: Alberto Fanton <alberto.fanton@protonmail.com>
--
-- This module contain the domain models
module Domain (SortMode) where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as B
import Data.Csv
  ( EncodeOptions (encUseCrLf),
    FromNamedRecord (..),
    Header,
    ToNamedRecord (..),
    decodeByName,
    defaultEncodeOptions,
    encodeByNameWith,
    namedRecord,
    (.:),
    (.=),
  )
import Data.OpenApi (ToParamSchema)
import Data.Text qualified as T
import Data.Vector qualified as V
import Servant (FromHttpApiData (parseQueryParam))
import System.Random (RandomGen)
import System.Random.Shuffle (shuffle')

data Sort where
  Random :: Sort
  MostRecent :: Sort
  Alphabetical :: Sort
  deriving stock (Generic)

newtype SortMode where
  SortMode :: Sort -> SortMode
  deriving stock (Generic)

instance ToParamSchema Sort

instance ToParamSchema SortMode

instance FromHttpApiData SortMode where
  parseQueryParam sortMode = case T.toTitle sortMode of
    "Random" -> Right $ SortMode Random
    "MostRecent" -> Right $ SortMode MostRecent
    "Alphabetical" -> Right $ SortMode Alphabetical
    _ -> Left "Unknown sort mode"

type Author = T.Text

type ISBN = T.Text

type Title = T.Text

supportedField :: [BS.ByteString]
supportedField = ["Title", "Author", "ISBN13"]

data Book = Book
  { title :: Title, -- the title of the book
    author :: Author, -- the author of the book
    isbn13 :: ISBN -- 13 digit book identifier that is intended to be unique
  }
  deriving (Show, Eq)

instance FromNamedRecord Book where
  parseNamedRecord r = do
    title <- r .: "Title"
    author <- r .: "Author"
    isbn13 <- r .: "ISBN13"
    return $ Book {..}

instance ToNamedRecord Book where
  toNamedRecord (Book {..}) =
    namedRecord
      [ "Title" .= title,
        "Author" .= author,
        "ISBN13" .= isbn13
      ]

parseBooks :: B.ByteString -> Either String (Header, V.Vector Book)
parseBooks = decodeByName

encodeBooks :: (Foldable f) => f Book -> B.ByteString
encodeBooks = encodeByNameWith encodeOptions (V.fromList supportedField) . toList
  where
    encodeOptions = defaultEncodeOptions {encUseCrLf = False}

-- Below, there is the business logic which could be moved away
randomBooks :: (RandomGen g) => [Book] -> State g [Book]
randomBooks books = gets (shuffle' books (length books))

-- TODO, this smells
-- apply :: (RandomGen g) => Operation -> g -> [Book] -> [Book]
-- apply Random gen books = evalState (randomBooks books) gen
-- apply _ _ books = books

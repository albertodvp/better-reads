{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Copyright: (c) 2023 Alberto Fanton
-- SPDX-License-Identifier: MIT
-- Maintainer: Alberto Fanton <alberto.fanton@protonmail.com>
--
-- This module contain the domain models
module Internal.Domain (Sort (..), Book (..), supportedBookFields) where

import Data.ByteString qualified as B
import Data.ByteString.Char8 (pack, unpack)
import Data.Csv
  ( FromField (..),
    FromNamedRecord (..),
    ToField (toField),
    ToNamedRecord (..),
    namedRecord,
    (.:),
    (.=),
  )
import Data.OpenApi (ToParamSchema)
import Data.Text qualified as T
import Data.Time (defaultTimeLocale, formatTime, parseTimeM)
import Data.Time.Calendar (Day)

data Sort where
  Random :: Sort
  MostRecent :: Sort
  Alphabetical :: Sort
  deriving stock (Generic)

instance ToParamSchema Sort

type Author = T.Text

type ISBN = T.Text

type Title = T.Text

dayFormat :: String
dayFormat = "%Y/%0m/%0d"

instance FromField Day where
  parseField = parseTimeM True defaultTimeLocale dayFormat . unpack

instance ToField Day where
  toField = pack . formatTime defaultTimeLocale dayFormat

supportedBookFields :: [B.ByteString]
supportedBookFields = ["Title", "Author", "ISBN13", "Date Added"]

data Book where
  Book :: {title :: Title, author :: Author, isbn13 :: ISBN, dateAdded :: Day} -> Book
  deriving stock (Show, Eq)

instance FromNamedRecord Book where
  parseNamedRecord r = do
    title <- r .: "Title"
    author <- r .: "Author"
    isbn13 <- r .: "ISBN13"
    dateAdded <- r .: "Date Added"
    return $ Book {..}

instance ToNamedRecord Book where
  toNamedRecord (Book {..}) =
    namedRecord
      [ "Title" .= title,
        "Author" .= author,
        "ISBN13" .= isbn13,
        "Date Added" .= dateAdded
      ]

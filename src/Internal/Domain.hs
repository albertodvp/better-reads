{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Copyright: (c) 2023 Alberto Fanton
-- SPDX-License-Identifier: MIT
-- Maintainer: Alberto Fanton <alberto.fanton@protonmail.com>
--
-- This module contain the domain models
module Internal.Domain (Sort (..), Book (..), supportedBookFields) where

import Data.ByteString qualified as B
import Data.Csv
  ( FromNamedRecord (..),
    ToNamedRecord (..),
    namedRecord,
    (.:),
    (.=),
  )
import Data.OpenApi (ToParamSchema)
import Data.Text qualified as T

data Sort where
  Random :: Sort
  MostRecent :: Sort
  Alphabetical :: Sort
  deriving stock (Generic)

instance ToParamSchema Sort

type Author = T.Text

type ISBN = T.Text

type Title = T.Text

supportedBookFields :: [B.ByteString]
supportedBookFields = ["Title", "Author", "ISBN13"]

data Book where
  Book :: {title :: Title, author :: Author, isbn13 :: ISBN} -> Book
  deriving stock (Show, Eq, Generic)

instance FromNamedRecord Book -- where
-- parseNamedRecord r = do
--   title <- r .: "Title"
--   author <- r .: "Author"
--   isbn13 <- r .: "ISBN13"
--   return $ Book {..}

instance ToNamedRecord Book where
  toNamedRecord (Book {..}) =
    namedRecord
      [ "Title" .= title,
        "Author" .= author,
        "ISBN13" .= isbn13
      ]

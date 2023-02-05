{- |
Copyright: (c) 2023 Alberto Fanton
SPDX-License-Identifier: MIT
Maintainer: Alberto Fanton <alberto.fanton@protonmail.com>

This module contain the domain models
-}
{-# LANGUAGE DerivingStrategies #-}

module Domain (Book(..), parseBooks) where

import qualified Data.ByteString.Lazy as B
import           Data.Csv
import qualified Data.Text            as T
import           Data.Vector

type Author = T.Text
type ISBN = T.Text
type Title = T.Text

data Book = Book {
    title  :: ISBN -- the title of the book
  , author :: Author -- the author of the book
  , isbn13 :: ISBN -- 13 digit book identifier that is intended to be unique
  } deriving stock (Show, Eq)

instance FromNamedRecord Book where
  parseNamedRecord r = Book <$> r  .: "Title" <*> r .: "Author" <*> r  .: "ISBN13"

data Operation = Random | List | GroupByCategory

parseBooks :: B.ByteString -> Either String (Header, Vector Book)
parseBooks = decodeByName

-- Below, there is the business logic which could be moved away

apply :: Foldable f => f Book -> Operation -> f Book
apply = undefined

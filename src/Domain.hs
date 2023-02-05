{- |
Copyright: (c) 2023 Alberto Fanton
SPDX-License-Identifier: MIT
Maintainer: Alberto Fanton <alberto.fanton@protonmail.com>

This module contain the domain models
-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards    #-}

module Domain (Book(..), parseBooks) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as B
import           Data.Csv
import           Data.Foldable        (foldr, toList)
import qualified Data.Text            as T
import qualified Data.Vector          as V
import           Prelude              hiding (lookup)

type Author = T.Text
type ISBN = T.Text
type Title = T.Text

supportedField :: [BS.ByteString]
supportedField = ["Title", "Author", "ISBN13"]

data Book = Book {
    title  :: ISBN -- the title of the book
  , author :: Author -- the author of the book
  , isbn13 :: ISBN -- 13 digit book identifier that is intended to be unique
  } deriving stock (Show, Eq)

instance FromNamedRecord Book where
  --parseNamedRecord r = Book <$> r .: "Title" <*> r .: "Author" <*> r .: "ISBN13"
  parseNamedRecord r = do
    title <- r .: "Title"
    author <- r .: "Author"
    isbn13 <- r .: "ISBN13"
    return $ Book {..}

data Operation = Random | List | GroupByCategory

parseBooks :: B.ByteString -> Either String (Header, V.Vector Book)
parseBooks = decodeByName

-- encodeBook :: Foldable f => f a -> B.ByteString
-- encodeBook = encodeByName (V.fromList supportedField) . toList


-- Below, there is the business logic which could be moved away

apply :: Foldable f => f Book -> Operation -> f Book
apply = undefined

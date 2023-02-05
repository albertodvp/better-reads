{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Copyright: (c) 2023 Alberto Fanton
SPDX-License-Identifier: MIT
Maintainer: Alberto Fanton <alberto.fanton@protonmail.com>

This module contain the domain models
-}
module Domain (Book (..), parseBooks, encodeBooks, Operation (..), apply) where

import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.Csv
import Data.Foldable (foldr, toList)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Random
import Prelude hiding (lookup)

type Author = T.Text
type ISBN = T.Text
type Title = T.Text

supportedField :: [BS.ByteString]
supportedField = ["Title", "Author", "ISBN13"]

data Book = Book
    { title :: ISBN -- the title of the book
    , author :: Author -- the author of the book
    , isbn13 :: ISBN -- 13 digit book identifier that is intended to be unique
    }
    deriving stock (Show, Eq)

instance FromNamedRecord Book where
    parseNamedRecord r = do
        title <- r .: "Title"
        author <- r .: "Author"
        isbn13 <- r .: "ISBN13"
        return $ Book{..}

instance ToNamedRecord Book where
    toNamedRecord (Book{..}) =
        namedRecord
            [ "Title" .= title
            , "Author" .= author
            , "ISBN13" .= isbn13
            ]

data Operation = Random | List deriving stock (Show)

parseBooks :: B.ByteString -> Either String (Header, V.Vector Book)
parseBooks = decodeByName

encodeBooks :: Foldable f => f Book -> B.ByteString
encodeBooks = encodeByNameWith encodeOptions (V.fromList supportedField) . toList
  where
    encodeOptions = defaultEncodeOptions{encUseCrLf = False}

-- Below, there is the business logic which could be moved away

randomBook :: (RandomGen g) => g -> V.Vector Book -> (V.Vector Book, g)
randomBook gen books = first selectPrepare random
  where
    random = uniformR (0, length books - 1) gen
    selectPrepare = V.singleton . (V.!) books

apply :: RandomGen g => g -> Operation -> V.Vector Book -> V.Vector Book
apply gen Random books =
    let (randomBookV, _gen) = randomBook gen books
     in randomBookV
apply _ _ books = books

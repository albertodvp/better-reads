{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Copyright: (c) 2023 Alberto Fanton
SPDX-License-Identifier: MIT
Maintainer: Alberto Fanton <alberto.fanton@protonmail.com>

This module contain the domain models
-}
module Domain (Book (..), parseBooks, encodeBooks, Operation (..), apply) where

import Control.Monad.State
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as B
import Data.Csv
import Data.Foldable (toList)
import Data.Functor ((<&>))
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Random
import System.Random.Shuffle
import Prelude hiding (State, evalState, lookup)

type Author = T.Text
type ISBN = T.Text
type Title = T.Text

supportedField :: [BS.ByteString]
supportedField = ["Title", "Author", "ISBN13"]

data Book = Book
    { title :: Title -- the title of the book
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

data Operation = Random | List deriving stock (Show, Read)

parseBooks :: B.ByteString -> Either String (Header, V.Vector Book)
parseBooks = decodeByName

encodeBooks :: Foldable f => f Book -> B.ByteString
encodeBooks = encodeByNameWith encodeOptions (V.fromList supportedField) . toList
  where
    encodeOptions = defaultEncodeOptions{encUseCrLf = False}

-- Below, there is the business logic which could be moved away
randomBooks :: RandomGen g => [Book] -> State g [Book]
randomBooks books = get <&> shuffle' books (length books)

-- TODO, this smells
apply :: RandomGen g => Operation -> g -> [Book] -> [Book]
apply Random gen books = evalState (randomBooks books) gen
apply _ _ books = books

module Service (sort, parseBooks, encodeBooks) where

import AppTypes (BooksFile (BooksFile), Limit, SortMode)
import Data.Csv
  ( EncodeOptions (encUseCrLf),
    decodeByName,
    defaultEncodeOptions,
    encodeByNameWith,
    header,
  )
import Data.Vector qualified as V
import Errors qualified as E
import Internal.Domain (Book, supportedBookFields)
import Prelude hiding (sort)

type EitherBR = Either E.Error

sort :: SortMode -> Limit -> BooksFile -> EitherBR BooksFile
sort sortMode limit booksFile = do
  books <- parseBooks booksFile
  return $ encodeBooks $ take limit books

parseBooks :: BooksFile -> EitherBR [Book]
parseBooks (BooksFile bs) = case decodeByName bs of
  Left _ -> Left E.CannotParseBooks
  Right (_, booksVector) -> Right $ V.toList booksVector

encodeBooks :: [Book] -> BooksFile
encodeBooks = BooksFile . encodeByNameWith encodeOptions h
  where
    h = header supportedBookFields
    encodeOptions = defaultEncodeOptions {encUseCrLf = False}

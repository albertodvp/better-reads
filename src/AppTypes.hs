{-
Maps types used in the web layer with types in the domain.
This mode does not expose any functionalities (exluded classes instancess).

-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module AppTypes (BooksFile (BooksFile), SortMode (SortMode), Limit, EitherBR, E.Error (..)) where

import Data.ByteString.Lazy qualified as B
import Data.OpenApi
  ( NamedSchema (NamedSchema),
    ToParamSchema,
    ToSchema (declareNamedSchema),
  )
import Data.Text qualified as T
import Errors qualified as E
import Internal.Domain (Sort (..))
import Servant
  ( FromHttpApiData (parseQueryParam),
    MimeRender (mimeRender),
    MimeUnrender (mimeUnrender),
    OctetStream,
  )

type EitherBR = Either E.Error

newtype BooksFile = BooksFile B.ByteString deriving stock (Generic, Show)

type Limit = Int

newtype SortMode where
  SortMode :: Sort -> SortMode
  deriving stock (Generic)

instance ToParamSchema SortMode

instance FromHttpApiData SortMode where
  parseQueryParam sortMode = case T.toTitle sortMode of
    "Random" -> Right $ SortMode Random
    "MostRecent" -> Right $ SortMode MostRecent
    "Alphabetical" -> Right $ SortMode Alphabetical
    _ -> Left "Unknown sort mode"

-- TODO
instance ToSchema BooksFile where
  declareNamedSchema _ = return $ NamedSchema (Just "File with books in csv format as extracted by good reads") mempty

instance MimeUnrender OctetStream BooksFile where
  mimeUnrender _ = Right . BooksFile

instance MimeRender OctetStream BooksFile where
  mimeRender _ (BooksFile bs) = bs

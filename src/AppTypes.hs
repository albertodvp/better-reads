{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module AppTypes where

import Data.ByteString.Lazy qualified as B
import Data.OpenApi
  ( NamedSchema (NamedSchema),
    ToSchema (declareNamedSchema),
  )
import Servant
  ( MimeRender (mimeRender),
    MimeUnrender (mimeUnrender),
    OctetStream,
  )

newtype BooksFile = BooksFile B.ByteString deriving stock (Generic)

-- TODO
instance ToSchema BooksFile where
  declareNamedSchema _ = return $ NamedSchema (Just "File with books in csv format as extracted by good reads") mempty

instance MimeUnrender OctetStream BooksFile where
  mimeUnrender _ = Right . BooksFile

instance MimeRender OctetStream BooksFile where
  mimeRender _ (BooksFile bs) = bs

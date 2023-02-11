{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module App (app) where

import Data.Proxy

-- import           Control.Monad.IO.Class (liftIO)
-- import           Domain                 (Book (..), Operation (..), apply,
--                                          encodeBooks, parseBooks)

import Common
import qualified Lucid as L
import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Wai
import qualified Network.Wai.Middleware.Gzip as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import Servant ((:<|>) (..), (:>))
import qualified Servant

-- TODO: check types
-- capitalize :: T.Text -> T.Text
-- capitalize = T.toTitle . T.toLower

-- paramErr :: T.Text
-- paramErr = "Invalid param"

-- newtype ParsedOperation = ParsedOperation Operation

-- instance FromHttpApiData ParsedOperation where
--     parseUrlPiece = f . capitalize
--       where
--         f "Random" = Right (ParsedOperation Random)
--         f "List"   = Right (ParsedOperation List)
--         f _        = Left paramErr

-- instance FromHttpApiData PingMode where
--     parseQueryParam = f . capitalize
--       where
--         f "Loud"   = Right Loud
--         f "Normal" = Right Normal
--         f _        = Left paramErr

-- TODO: what is happening here?
--       - what is '[OctetStream] (DataKinds)
--       - does (:<|>), (:>) work (TypeOperators)
-- type BookOperationAPI = "booksOperation"
--                         :> Capture "operation" ParsedOperation
--                         :> QueryParam "limit" Int
--                         :> ReqBody '[OctetStream] B.ByteString
--                         :> Post '[OctetStream] B.ByteString

type API =
    -- StaticAPI
    -- :<|> BookOperationAPI
    -- :<|> ServerRoutes
    -- :<|> Servant.Raw
    Servant.Raw

-- applyOperation :: Operation -> [Book] -> Handler [Book]
-- applyOperation op books = do
--     gen <- liftIO initStdGen
--     pure $ apply op gen books

-- booksOperationHandler :: ParsedOperation -> Maybe Int -> B.ByteString -> Handler B.ByteString
-- booksOperationHandler pOp Nothing booksBS = booksOperationHandler pOp (Just 1) booksBS
-- booksOperationHandler (ParsedOperation op) (Just n) booksBS = case parseBooks booksBS of
--     Left _ -> throwError $ err400{errBody = "Bad request :("}
--     Right (_, books) -> encodeBooks . take n <$> applyOperation op (toList books)
app :: Wai.Application
app =
    Servant.serve (Proxy @API) (Servant.Tagged page404)
  where
    page404 :: Wai.Application
    page404 _ respond =
        respond
            $ Wai.responseLBS
                HTTP.status404
                [("Content-Type", "text/plain")]
            $ L.renderBS
            $ L.toHtml Common.page404View

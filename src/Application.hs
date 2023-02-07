{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Application (app) where

import qualified Data.ByteString.Lazy as B
import Data.Text as T
import Domain (
    Book (..),
    Operation (..),
    encodeBooks,
    parseBooks,
 )
import Servant
import Prelude hiding (ByteString)

data PingMode = Loud | Normal

capitalize :: Text -> Text
capitalize = T.toTitle . T.toLower

paramErr :: Text
paramErr = "Invalid param"
instance FromHttpApiData Operation where
    parseUrlPiece = f . capitalize
      where
        f "Random" = Right Random
        f "List" = Right List
        f _ = Left paramErr

instance FromHttpApiData PingMode where
    parseQueryParam = f . capitalize
      where
        f "Loud" = Right Loud
        f "Normal" = Right Normal
        f _ = Left paramErr

-- TODO: what is happening here?
--       - what is '[JSON] (DataKinds)
--       - does (:<|>), (:>) work (TypeOperators)
type API =
    Get '[JSON] String
        :<|> "ping" :> QueryParam "mode" PingMode :> Get '[JSON] String
        -- TODO: implement this by yourself
        :<|> "booksOperation" :> Capture "operation" Operation :> ReqBody '[OctetStream] B.ByteString :> Post '[OctetStream] B.ByteString

handlerPingPong :: Maybe PingMode -> Handler String
handlerPingPong mm =
    let res = case mm of
            Just Loud -> "PONG"
            Just Normal -> "pong"
            Nothing -> "..."
     in pure res

booksOperationHandler :: Operation -> B.ByteString -> Handler B.ByteString
booksOperationHandler _ booksBS = case parseBooks booksBS of
    Left err -> throwError $ err400{errBody = "Bad request :("}
    Right (_, books) -> pure $ encodeBooks books

server :: Server API
server =
    return "Hello, servant!"
        :<|> handlerPingPong
        :<|> booksOperationHandler

rootApi :: Proxy API
rootApi = Proxy

app :: Application
app = serve rootApi server

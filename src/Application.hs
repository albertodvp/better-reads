{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Application (app) where

import qualified Data.ByteString.Lazy as B
import Data.Text as T
import Data.Vector as V
import Domain (
    Book (..),
    Operation (..),
    apply,
    encodeBooks,
    parseBooks,
 )
import Servant
import System.Random
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

applyOperation :: Operation -> V.Vector Book -> Handler B.ByteString
applyOperation op books = do
    gen <- liftIO initStdGen
    pure $ encodeBooks $ apply gen op books

booksOperationHandler :: Operation -> B.ByteString -> Handler B.ByteString
booksOperationHandler op booksBS = case parseBooks booksBS of
    Left err -> throwError $ err400{errBody = "Bad request :("}
    Right (_, books) -> applyOperation op books

server :: Server API
server =
    return "Hello, servant!"
        :<|> handlerPingPong
        :<|> booksOperationHandler

rootApi :: Proxy API
rootApi = Proxy

app :: Application
app = serve rootApi server

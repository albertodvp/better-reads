{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module App (app) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as B
import Data.Foldable (toList)
import qualified Data.Text as T
import Domain (
    Book (..),
    Operation (..),
    apply,
    encodeBooks,
    parseBooks,
 )
import Servant
import System.Random

data PingMode = Loud | Normal

capitalize :: T.Text -> T.Text
capitalize = T.toTitle . T.toLower

paramErr :: T.Text
paramErr = "Invalid param"

newtype ParsedOperation = ParsedOperation Operation

instance FromHttpApiData ParsedOperation where
    parseUrlPiece = f . capitalize
      where
        f "Random" = Right (ParsedOperation Random)
        f "List" = Right (ParsedOperation List)
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
        :<|> "booksOperation" :> Capture "operation" ParsedOperation :> QueryParam "limit" Int :> ReqBody '[OctetStream] B.ByteString :> Post '[OctetStream] B.ByteString

handlerPingPong :: Maybe PingMode -> Handler String
handlerPingPong mm =
    let res = case mm of
            Just Loud -> "PONG"
            Just Normal -> "pong"
            Nothing -> "..."
     in pure res

applyOperation :: Operation -> [Book] -> Handler [Book]
applyOperation op books = do
    gen <- liftIO initStdGen
    pure $ apply op gen books

booksOperationHandler :: ParsedOperation -> Maybe Int -> B.ByteString -> Handler B.ByteString
booksOperationHandler pOp Nothing booksBS = booksOperationHandler pOp (Just 1) booksBS
booksOperationHandler (ParsedOperation op) (Just n) booksBS = case parseBooks booksBS of
    Left _ -> throwError $ err400{errBody = "Bad request :("}
    Right (_, books) -> encodeBooks . take n <$> applyOperation op (toList books)

server :: Server API
server =
    return "Hello, servant!"
        :<|> handlerPingPong
        :<|> booksOperationHandler

rootApi :: Proxy API
rootApi = Proxy

app :: Application
app = serve rootApi server

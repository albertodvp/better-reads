{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module App (app) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as B
import Data.Foldable (toList)

import Data.String (IsString (..), fromString)
import qualified Data.Text as T
import Domain (
    Book (..),
    Operation (..),
    apply,
    encodeBooks,
    parseBooks,
 )
import qualified Lucid as L

import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import Servant
import Servant.HTML.Lucid (HTML)
import System.Random
import Prelude hiding (index)

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
type StaticAPI = "static" :> Raw
type RootGetAPI = Get '[HTML] (L.Html ())
type RootPostAPI = Post '[HTML] String

type API =
    RootGetAPI
        :<|> StaticAPI
        :<|> "ping" :> QueryParam "mode" PingMode :> Get '[JSON] String
        -- TODO: implement this by yourself
        :<|> "booksOperation" :> Capture "operation" ParsedOperation :> QueryParam "limit" Int :> ReqBody '[OctetStream] B.ByteString :> Post '[OctetStream] B.ByteString
        :<|> Raw

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

page404App :: Wai.Application
page404App _ respond =
    respond
        $ Wai.responseLBS
            HTTP.status404
            [("Content-Type", "text/html")]
        $ L.renderBS page404

server :: Server API
server =
    return pageIndex
        :<|> staticServer
        :<|> handlerPingPong
        :<|> booksOperationHandler
        -- TODO what is servant.tagged
        :<|> Servant.Tagged page404App

staticServer :: Server StaticAPI
staticServer = Servant.serveDirectoryFileServer "static"

app :: Wai.Application
app = serve (Proxy @API) server

-- Pagees
mkPage :: Monad m => L.HtmlT m b -> L.HtmlT m b
mkPage body = do
    L.doctype_
    L.head_ $ do
        L.link_ [L.rel_ "stylesheet", L.href_ "https://fonts.googleapis.com/css?family=Roboto:300,300italic,700,700italic"]
        L.link_ [L.rel_ "stylesheet", L.href_ "https://cdnjs.cloudflare.com/ajax/libs/normalize/8.0.1/normalize.css"]
        L.link_ [L.rel_ "stylesheet", L.href_ "https://cdnjs.cloudflare.com/ajax/libs/milligram/1.4.1/milligram.css"]

        L.link_ [L.rel_ "stylesheet", L.href_ "/static/style.css"]
        L.script_ [L.src_ "static/script.js"] ("" :: String) -- TODO how do I remove this call?
        L.title_ "Better-reads"
        L.meta_ [L.charset_ "utf-8"]
    L.body_ $ L.div_ [L.class_ "container"] $ do
        L.h1_ $ mconcat ["BetterReads:", L.br_ [], "a toolkit to handle goodreads books, ", L.strong_ "better"]
        L.div_ [L.class_ ""] body

page404 :: L.Html ()
page404 = mkPage $ L.p_ "Page not found :("

pageIndex :: L.Html ()
toIsString :: (Show a, IsString b) => a -> b
toIsString = fromString . show
pageIndex =
    mkPage $
        L.form_ [L.id_ "form"] $
            mconcat
                [ L.label_ [L.for_ "booksFile"] "Goodreads export file:"
                , L.input_ [L.name_ "booksFile", L.type_ "file", L.id_ "booksFile", L.required_ ""]
                , L.label_ [L.for_ "operation"] "Operation:"
                , L.select_ [L.name_ "operation", L.id_ "operation"] $ mconcat $ (\op -> L.option_ [L.value_ $ toIsString op] $ toIsString op) <$> [Random, List]
                , L.label_ [L.for_ "limit"] "Number of books (in the output):"
                , L.input_ [L.name_ "limit", L.type_ "number", L.id_ "limit", L.required_ "", L.value_ "1"]
                , L.input_ [L.type_ "reset", L.class_ "button button-outline form-button"]
                , L.input_ [L.type_ "submit", L.class_ "button button-clear form-button"]
                ]

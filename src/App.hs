{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module App (app) where

import Data.ByteString.Lazy qualified as B
import Data.OpenApi (NamedSchema (NamedSchema), OpenApi, ToParamSchema, ToSchema (declareNamedSchema))
import Data.Proxy (Proxy)
import Data.Text qualified as T
import Domain
  ( Book (..),
    Operation (..),
    apply,
    encodeBooks,
    parseBooks,
  )
import GHC.Generics (Generic)
import Lucid qualified as L
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai
import Servant
import Servant.API (JSON)
import Servant.HTML.Lucid (HTML)
import Servant.OpenApi
import System.Random

data PingMode = Loud | Normal

capitalize :: T.Text -> T.Text
capitalize = T.toTitle . T.toLower

paramErr :: T.Text
paramErr = "Invalid param"

newtype ParsedOperation = ParsedOperation Operation deriving (Generic)

instance ToParamSchema ParsedOperation

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

type RootGetAPI = Get '[HTML] (L.Html ())

newtype BooksFile = BooksFile B.ByteString deriving (Generic)

instance ToSchema BooksFile where
  declareNamedSchema _ = return $ NamedSchema (Just "File with books") $ mempty

instance MimeRender OctetStream BooksFile where
  mimeRender _ (BooksFile bs) = bs

instance MimeUnrender OctetStream BooksFile where
  mimeUnrender _ = Right . BooksFile

type API =
  "api"
    :> ( "healthcheck" :> Get '[PlainText] String
           :<|> "booksOperation" :> Capture "operation" ParsedOperation :> QueryParam "limit" Int :> ReqBody '[OctetStream] BooksFile :> Post '[OctetStream] BooksFile
       )

type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi

type StaticAPI = "static" :> Raw

type App =
  RootGetAPI
    :<|> StaticAPI
    :<|> API
    :<|> SwaggerAPI
    :<|> Raw

applyOperation :: Operation -> [Book] -> Handler [Book]
applyOperation op books = do
  gen <- liftIO initStdGen
  pure $ apply op gen books

booksOperationHandler :: ParsedOperation -> Maybe Int -> BooksFile -> Handler BooksFile
booksOperationHandler pOp Nothing booksFile = booksOperationHandler pOp (Just 1) booksFile
booksOperationHandler (ParsedOperation op) (Just n) (BooksFile booksBS) = case parseBooks booksBS of
  Left _ -> throwError $ err400 {errBody = "Bad request :("}
  Right (_, books) -> BooksFile . encodeBooks . take n <$> applyOperation op (toList books)

page404App :: Wai.Application
page404App _ respond =
  respond
    $ Wai.responseLBS
      HTTP.status404
      [("Content-Type", "text/html")]
    $ L.renderBS page404

api :: Proxy API
api = Proxy

swagger :: OpenApi
swagger = toOpenApi api

server :: Server App
server =
  return pageIndex
    :<|> staticServer
    :<|> ( return "ok"
             :<|> booksOperationHandler
         )
    :<|> return swagger
    -- TODO what is servant.tagged
    :<|> Servant.Tagged page404App

staticServer :: Server StaticAPI
staticServer = Servant.serveDirectoryFileServer "static"

app :: Wai.Application
app = serve (Proxy @App) server

-- Pagees
mkPage :: (Monad m) => L.HtmlT m b -> L.HtmlT m b
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
        [ L.label_ [L.for_ "booksFile"] "Goodreads export file:",
          L.input_ [L.name_ "booksFile", L.type_ "file", L.id_ "booksFile", L.required_ ""],
          L.label_ [L.for_ "operation"] "Operation:",
          L.select_ [L.name_ "operation", L.id_ "operation"] $ mconcat $ (\op -> L.option_ [L.value_ $ toIsString op] $ toIsString op) <$> [Random, List],
          L.label_ [L.for_ "limit"] "Number of books (in the output):",
          L.input_ [L.name_ "limit", L.type_ "number", L.id_ "limit", L.required_ "", L.value_ "1"],
          L.input_ [L.type_ "reset", L.class_ "button button-outline form-button"],
          L.input_ [L.type_ "submit", L.class_ "button button-clear form-button"]
        ]

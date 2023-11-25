{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module App (app) where

import AppTypes (BooksFile)
import Data.OpenApi (OpenApi)
import Domain (SortMode)
import Lucid qualified as L
import Network.Wai qualified as Wai
import Servant
import Servant.HTML.Lucid (HTML)
import Servant.OpenApi
import Static (page404App, pageIndex)

{-

API Types

-}
type RootGetAPI = Get '[HTML] (L.Html ())

type API =
  "api"
    :> ( "healthcheck" :> Get '[PlainText] String
           :<|> "sort" :> QueryParam' '[Required] "sort-mode" SortMode :> QueryParam "limit" Int :> ReqBody '[OctetStream] BooksFile :> Post '[OctetStream] BooksFile
       )

type SwaggerAPI = "swagger.json" :> Get '[JSON] OpenApi

type App =
  RootGetAPI
    :<|> "static" :> Raw
    :<|> API
    :<|> SwaggerAPI
    :<|> Raw

{-

API Handers

-}
sortHandler :: SortMode -> Maybe Int -> BooksFile -> Handler BooksFile
sortHandler sortMode Nothing booksFile = sortHandler sortMode (Just 1) booksFile
sortHandler sortMode (Just limit) booksFile = undefined

server :: Server App
server =
  return pageIndex
    :<|> Servant.serveDirectoryFileServer "static"
    :<|> ( return "ok"
             :<|> sortHandler
         )
    :<|> return (toOpenApi (Proxy :: Proxy API))
    :<|> Servant.Tagged page404App

app :: Wai.Application
app = serve (Proxy @App) server

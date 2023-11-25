module Static (pageIndex, page404App) where

import Lucid qualified as L
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai

{-
Pages
-}
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
pageIndex = return ()

{-
Applications
-}

page404App :: Wai.Application
page404App _ responder =
  responder
    $ Wai.responseLBS
      HTTP.status404
      [("Content-Type", "text/html")]
    $ L.renderBS page404

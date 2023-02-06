{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Application (app) where

import Data.Text as T
import Servant

data PingMode = Loud | Normal

instance FromHttpApiData PingMode where
    parseQueryParam = f . T.toTitle . T.toLower
      where
        f "Loud" = Right Loud
        f "Normal" = Right Normal
        f _ = Left "Invalid argument"

-- TODO: what is happening here?
--       - what is '[JSON] (DataKinds)
--       - does (:<|>), (:>) work (TypeOperators)
type API = Get '[JSON] String :<|> "ping" :> QueryParam "mode" PingMode :> Get '[JSON] String

server :: Server API
server = return "Hello, servant!" :<|> return . f
  where
    f (Just Loud) = "PONG"
    f (Just Normal) = "pong"
    f Nothing = "..."

rootApi :: Proxy API
rootApi = Proxy

app :: Application
app = serve rootApi server

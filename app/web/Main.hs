module Main (main) where

import App (app)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import qualified Network.Wai.Handler.Warp as Wai
import qualified Network.Wai.Middleware.Gzip as Wai
import qualified Network.Wai.Middleware.RequestLogger as Wai
import qualified System.IO as IO

oldMain :: IO ()
oldMain = run 8081 app

main :: IO ()
main = do
    IO.hPutStrLn IO.stderr $ "Running on port " ++ show port
    Wai.run port $ Wai.logStdout $ compress app
  where
    port = 3003
    compress :: Wai.Middleware
    compress = Wai.gzip Wai.def{Wai.gzipFiles = Wai.GzipCompress}

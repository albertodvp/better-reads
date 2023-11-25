module Main (main) where

import App (app)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import System.IO (hPutStrLn)

main :: IO ()
main = do
  port <- fromMaybe 8080 . (>>= readMaybe) <$> lookupEnv "PORT"
  hPutStrLn stderr $ "Running on port: " ++ show port
  run port $ logStdout app

module Main (main) where

import App (app)
import Data.Maybe (fromMaybe)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdout)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
    port <- fromMaybe 8123 . (>>= readMaybe) <$> lookupEnv "PORT"
    hPutStrLn stderr $ "Running on port: " ++ show port
    run port $ logStdout app

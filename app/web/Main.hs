module Main (main) where

import           App                                  (app)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (logStdout)
import           System.IO                            (hPutStrLn, stderr)

main :: IO ()
main = do
    let port = 80
    hPutStrLn stderr $ "Running on port " ++ show port
    run port $ logStdout app

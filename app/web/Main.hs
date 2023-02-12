module Main (main) where

import App (app)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8081 app

--

-- main :: IO ()
-- main = do
--   let port = 3003
--   IO.hPutStrLn IO.stderr $ "Running on port " ++ show port

--   Wai.run

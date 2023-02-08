module Main (main) where

import App (app)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8081 app

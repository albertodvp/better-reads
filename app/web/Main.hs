module Main (main) where

import Application (app)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8081 app

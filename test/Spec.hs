module Main (main) where

import BetterReads (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)

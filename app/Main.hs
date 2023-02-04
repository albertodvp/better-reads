module Main (main) where

import BetterReads (projectName)


main :: IO ()
main = putStrLn ("Executable for " ++ projectName)

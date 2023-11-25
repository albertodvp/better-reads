{-# LANGUAGE ViewPatterns #-}

module Main (main) where

-- import Data.ByteString.Lazy qualified as BL
-- import Domain
--   ( Operation (..),
--     apply,
--     encodeBooks,
--     parseBooks,
--   )
-- import Options.Applicative
-- import System.Random

-- type Limit = Int

-- randomO :: Parser Operation
-- randomO = flag' Random (long "random" <> help "Get a list with only one random book")

-- listO :: Parser Operation
-- listO = flag' List (long "list" <> help "Get the same list you passed")

-- operation :: Parser Operation
-- operation = randomO <|> listO

-- params :: Parser (FilePath, FilePath, Limit, Operation)
-- params =
--   (,,,)
--     <$> argument str (metavar "INPUT_BOOKS_FILE")
--     <*> argument str (metavar "OUTPUT_BOOKS_FILE")
--     <*> option auto (long "limit" <> short 'l' <> metavar "LIMIT" <> help "The number of books")
--     <*> operation

-- opts :: ParserInfo (FilePath, FilePath, Limit, Operation)
-- opts = info (params <**> helper) (fullDesc <> progDesc "Do something with you good read books" <> header "BetterReads - A toolkit to better handle goodread books")

main :: IO ()
main = undefined

-- main :: IO ()
-- main = do
--   (inputFilePath, outputFilePath, limit, op) <- execParser opts
--   csvData <- BL.readFile inputFilePath
--   randomGen <- initStdGen
--   case parseBooks csvData of
--     Left err -> putStrLn err
--     Right (_, toList -> books) ->
--       BL.writeFile outputFilePath $ encodeBooks $ take limit (apply op randomGen books)

module Main (main) where
import           Data.ByteString.Lazy as BL
import           Data.Foldable        (traverse_)
import           Data.Semigroup       ((<>))
import           Domain               (Operation (..), apply, parseBooks)
import           Options.Applicative
import           System.Random

randomO :: Parser Operation
randomO = flag' Random (long "random" <> help "Get a list with only one random book")
listO :: Parser Operation
listO = flag' List (long "list" <> help "Get the same list you passed")

operation :: Parser Operation
operation = randomO <|> listO

params :: Parser (FilePath, Operation)
params = (,) <$> argument str (metavar "INPUT_BOOKS_FILE") <*> operation

opts :: ParserInfo (FilePath, Operation)
opts = info (params <**> helper) (fullDesc <> progDesc "Do something with you good read books" <> header "BetterReads - A toolkit to better handle goodread books")

main :: IO ()
main = do
  (filePath, operation) <- execParser opts
  csvData <- BL.readFile filePath
  randomGen <- initStdGen
  case parseBooks csvData of
    Left err         -> putStrLn err
    Right (_, books) -> traverse_ print (apply randomGen operation books)


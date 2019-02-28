module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Options.Applicative (optional)
import Text.Read (readMaybe)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(..))

import Language.Netsim


-- | Parses a network represented as a string
-- | Links must be split by newlines
-- | Each link must have format 'alpha beta cost' where 'cost' is an integer
parseNetwork :: String -> Maybe [(String, String, Int)]
parseNetwork s = mapM (parseRow . words) (lines s)
    where parseRow :: [String] -> Maybe (String, String, Int)
          parseRow [a, b, c] = case readMaybe c :: Maybe Int of
                                 Nothing -> Nothing
                                 Just c' -> Just (a, b, c')
          parseRow _         = Nothing

data Command
    = Tick
    | Cost Int String String Int
    | Fail Int String String
    | Route String String
    | Table String String
    | SplitHorizon

data Configuration = Configuration
    { filepath :: String
    , directed :: Bool
    , iterations :: Maybe Int
    } deriving Show

configuration :: Parser Configuration
configuration = Configuration
    <$> (strOption $ long "file" <> short 'f' <> metavar "FILE")
    <*> (switch $ long "directed" <> short 'd')
    <*> (optional $ option auto $ long "iterations" <> short 'i' <> metavar "NUM")

opts :: ParserInfo Configuration
opts = info (configuration <**> helper)
     ( fullDesc
    <> progDesc "Blah"
    <> header "bla bla"
     )

exitWithErr :: String -> Int -> IO ()
exitWithErr s e = hPutStrLn stderr s >> exitWith (ExitFailure e)

execute :: Configuration -> IO ()
execute conf = do ns <- readFile (filepath conf)
                  case parseNetwork ns of
                    Nothing -> exitWithErr "Error: could not parse network file" 1
                    Just n -> do let net = (if directed conf then id else bidir) (unflatten n)
                                 putStrLn (printNetwork net)


main :: IO ()
main = execute =<< execParser opts

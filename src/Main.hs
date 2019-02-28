module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe, listToMaybe)
import Text.Read (readMaybe)
import System.IO (hPutStrLn, hFlush, stdout, stderr)
import System.Exit (exitFailure, exitSuccess)

import Language.Netsim


-- | Parses a network represented as a string
-- | Links must be split by newlines
-- | Each link must have format 'alpha beta cost' where 'cost' is an integer
parseNetwork :: String -> Maybe [(Node, Node, Cost)]
parseNetwork s = mapM (parseRow . words) (lines s)
    where parseRow [a, b, c] = case readMaybe c :: Maybe Int of
                                 Nothing -> Nothing
                                 Just c' -> Just (a, b, c')
          parseRow _         = Nothing

{---------------------}
{- INTERACTIVE SHELL -}
{---------------------}

data Command
    = Exit
    | Tick
    | Run
    | RunFor Int
    | Cost String String Int
    | Fail String String
    | Route String String
    | Table String
    | SplitHorizon

parseCommand :: [String] -> Maybe Command
parseCommand ["exit"] = Just Exit
parseCommand ["tick"] = Just Tick
parseCommand ["run"] = Just Run
parseCommand ["run", i] = RunFor <$> (readMaybe i :: Maybe Int)
parseCommand ["cost", s, d, i] = Cost s d <$> (readMaybe i :: Maybe Int)
parseCommand ["fail", s, d] = Just $ Fail s d
parseCommand ["route", s, d] = Just $ Route s d
parseCommand ["table", s] = Just $ Table s
parseCommand ["split-horizon"] = Just SplitHorizon
parseCommand _ = Nothing

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast [x]    = Just x
safeLast (x:xs) = safeLast xs

doCommand :: Command -> (GetDV, Network, State) -> IO (GetDV, Network, State)
doCommand Exit         (f, net, st) = exitSuccess
doCommand Tick         (f, net, st) = pure (f, net, tick f net st)
doCommand Run          (f, net, st) = pure (f, net, fromMaybe st $ safeLast $ run f net st)
doCommand (RunFor n)   (f, net, st) = pure (f, net, fromMaybe st $ safeLast $ runN n f net st)
doCommand (Cost s d n) (f, net, st) = pure (f, alter s d (const n) net, st)
doCommand (Fail s d)   (f, net, st) = pure (f, disconnect s d net, st)
doCommand (Route s d)  (f, net, st) = putStrLn (printRoute (route s d st)) >> pure (f, net, st)
doCommand (Table s)    (f, net, st) = putStrLn (printTable (rtable s st)) >> pure (f, net, st)
doCommand SplitHorizon (_, net, st) = pure (wSH, net, st)

commandHelp :: String
commandHelp = unlines
    [ "exit             exit the simulation"
    , "tick             advance the simulation by a single tick"
    , "run              run the simulation until state is stable"
    , "run N            run the simulation for N steps or until state is stable"
    , "cost S D N       set the cost of the link S -> D to N (NOTE: D -> S is left unchanged)"
    , "fail S D         destroy the link S -> D (NOTE: D -> S is left unchanged)"
    , "route S D        display the best route from S to D"
    , "table S          display the routing table for node S"
    , "split-horizon    activate split-horizon on all nodes"
    ]

runPrompt :: (GetDV, Network, State) -> IO ()
runPrompt s = do putStr "> "
                 hFlush stdout
                 tokens <- words <$> getLine
                 case parseCommand tokens of
                   Just c -> doCommand c s >>= runPrompt
                   Nothing -> putStrLn commandHelp >> runPrompt s

{-------}
{- CLI -}
{-------}

data Configuration = Configuration
    { filepath :: String
    , directed :: Bool
    } deriving Show

configuration :: Parser Configuration
configuration = Configuration
    <$> (strOption $ long "file" <> short 'f' <> metavar "FILE")
    <*> (switch $ long "directed" <> short 'd')

opts :: ParserInfo Configuration
opts = info (configuration <**> helper)
     ( fullDesc
    <> progDesc "Blah"
    <> header "bla bla"
     )

exitWithErr :: String -> Int -> IO ()
exitWithErr s e = hPutStrLn stderr s >> exitFailure

execute :: Configuration -> IO ()
execute conf = do ns <- readFile (filepath conf)
                  case parseNetwork ns of
                    Nothing -> exitWithErr "Error: could not parse network file" 1
                    Just n -> do let net = (if directed conf then id else bidir) (unflatten n)
                                 runPrompt (woSH, net, state net)

main :: IO ()
main = execute =<< execParser opts

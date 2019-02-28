module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Maybe (fromMaybe, listToMaybe)
import Text.Read (readMaybe)
import System.IO (hPutStrLn, hFlush, stdout, stderr)
import System.Exit (exitFailure, exitSuccess)

import Language.Netsim

safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast [x]    = Just x
safeLast (x:xs) = safeLast xs

{---------------------}
{- INTERACTIVE SHELL -}
{---------------------}

data Comm
    = Exit
    | Tick
    | Run
    | RunFor Int
    | Cost String String Int
    | Fail String String
    | Route String String
    | Table String
    | SplitHorizon

parseComm :: [String] -> Maybe Comm
parseComm ["exit"]          = Just Exit
parseComm ["tick"]          = Just Tick
parseComm ["run"]           = Just Run
parseComm ["run", i]        = RunFor <$> (readMaybe i :: Maybe Int)
parseComm ["cost", s, d, i] = Cost s d <$> (readMaybe i :: Maybe Int)
parseComm ["fail", s, d]    = Just $ Fail s d
parseComm ["route", s, d]   = Just $ Route s d
parseComm ["table", s]      = Just $ Table s
parseComm ["split-horizon"] = Just SplitHorizon
parseComm _                 = Nothing

doComm :: Comm -> (GetDV, Network, State) -> IO (GetDV, Network, State)
doComm Exit         (f, net, st) = exitSuccess
doComm Tick         (f, net, st) = pure (f, net, tick f net st)
doComm Run          (f, net, st) = pure (f, net, fromMaybe st $ safeLast $ run f net st)
doComm (RunFor n)   (f, net, st) = pure (f, net, fromMaybe st $ safeLast $ runN n f net st)
doComm (Cost s d n) (f, net, st) = pure (f, alter s d (const n) net, st)
doComm (Fail s d)   (f, net, st) = pure (f, disconnect s d net, st)
doComm (Route s d)  (f, net, st) = putStrLn (printRoute (route s d st)) >> pure (f, net, st)
doComm (Table s)    (f, net, st) = putStrLn (printTable (rtable s st)) >> pure (f, net, st)
doComm SplitHorizon (_, net, st) = pure (wSH, net, st)

commHelp :: String
commHelp = unlines
    [ "exit             exit the simulation"
    , "tick             advance the simulation by a single tick"
    , "run              run the simulation until state is stable"
    , "run N            run the simulation for N steps or until state is stable"
    , "cost S D N       set the cost of the link S -> D to N"
    , "                 NOTE: D -> S is left unchanged"
    , "fail S D         destroy the link S -> D"
    "                   NOTE: D -> S is left unchanged"
    , "route S D        display the best route from S to D"
    , "table S          display the routing table for node S"
    , "split-horizon    activate split-horizon on all nodes"
    ]

runPrompt :: (GetDV, Network, State) -> IO ()
runPrompt s = do putStr "> "
                 hFlush stdout
                 tokens <- words <$> getLine
                 case parseComm tokens of
                   Just c -> doComm c s >>= runPrompt
                   Nothing -> putStrLn commHelp >> runPrompt s

{------------------------}
{- NETWORK FILE PARSING -}
{------------------------}

-- | Parses a network represented as a string
-- | Links must be split by newlines
-- | Each link must have format 'alpha beta cost' where 'cost' is an integer
parseNetwork :: String -> Maybe [(Node, Node, Cost)]
parseNetwork s = mapM (parseRow . words) (lines s)
    where parseRow [a, b, c] = case readMaybe c :: Maybe Int of
                                 Nothing -> Nothing
                                 Just c' -> Just (a, b, c')
          parseRow _         = Nothing

{--------------------------}
{- COMMAND LINE INTERFACE -}
{--------------------------}

data Configuration = Configuration
    { filepath :: String
    , directed :: Bool
    }

configuration :: Parser Configuration
configuration = Configuration
    <$> (argument str $ metavar "NETWORK-FILE")
    <*> (switch $ long "directed" <> short 'd' <> help "Network edges are directed")

opts :: ParserInfo Configuration
opts = info (configuration <**> helper)
     ( fullDesc
    <> header "Netsim - Uma Zalakain"
    <> progDesc (unlines
        [ "Parses NETWORK-FILE and jumps into an interactive shell."
        , "Each line in the file represents a link and must have the following syntax:"
        , "<SOURCE> <DESTINATION> <COST>"
        , "where <SOURCE> and <DESTINATION> are arbitrary identifiers and <COST> is an integer."
        ])
     )

execute :: Configuration -> IO ()
execute conf = do
    ns <- readFile (filepath conf)
    case parseNetwork ns of
      Nothing -> hPutStrLn stderr "Error: could not parse network file" >> exitFailure
      Just n -> do let net = (if directed conf then id else bidir) (unflatten n)
                   runPrompt (woSH, net, state net)

main :: IO ()
main = execute =<< execParser opts

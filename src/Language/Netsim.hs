{-# LANGUAGE DeriveGeneric, TupleSections #-}

module Language.Netsim
    ( Node
    , Source
    , Destination
    , Gateway
    , Cost
    , Network
    , State
    , Ads
    , unflatten
    , bidir
    , state
    , woSH
    , wSH
    , send
    , receive
    , tick
    , run
    , runN
    , printNetwork
    , printState
    , printAds
    ) where

import qualified Data.HashMap as M
import Data.Maybe (fromMaybe)
import Data.Hashable (Hashable)
import Text.Printf (printf)

{---------}
{- TYPES -}
{---------}

-- These are all equivalent type synonyms, used for clarity

type Node        = String
type Source      = Node
type Destination = Node
type Gateway     = Node
type Cost        = Int

-- Double index: a map inside a map
type DI a b c = M.Map a (M.Map b c)

-- Network links are generalised to be directional:
-- A -> B and B -> A can have different costs
type Network = DI Source Destination Cost

-- Only routing tables are stateful
type State   = DI Source Destination (Cost, Gateway)
type RTable  = M.Map Destination (Cost, Gateway)

-- Advertisements sent by nodes contain distance vectors
type Ads     = DI Source Destination DV
type DV      = M.Map Destination Cost

{--------------------------}
{- STRUCTURAL COMPUTATION -}
{--------------------------}

-- | Swap the indices in a nested map
-- | This will allow us to index by either source or destination
swap :: (Hashable a, Ord a, Hashable b, Ord b) => DI a b c -> DI b a c
swap = M.unionsWith M.union
     . M.elems
     . M.mapWithKey (\n -> M.map (\a -> M.singleton n a))

-- | Flatten a nested map into tuples
flatten :: DI a b c -> [(a, b, c)]
flatten = concatMap (M.elems) . M.elems . M.mapWithKey (\a -> M.mapWithKey (\b c -> (a, b, c)))

-- | Build a nested map from tuples
unflatten :: (Hashable a, Ord a, Hashable b, Ord b) => [(a, b, c)] -> DI a b c
unflatten = foldr (\(a, b, c) -> M.insertWith M.union a (M.singleton b c)) M.empty

-- | Add indices in both directions: A -> B -> X iff B -> A -> X
bidir :: (Hashable a, Ord a) => DI a a b -> DI a a b
bidir net = M.unionWith M.union net (swap net)

{------------------}
{- INITIALIZATION -}
{------------------}

-- | Initialise state: each node knows about its neighbours
state :: Network -> State
state = M.map (M.mapWithKey (\d c -> (c, d)))

{---------------}
{- ADVERTISING -}
{---------------}

-- | Obtain the routing table for a given node
rtable :: Source -> State -> RTable
rtable s = M.findWithDefault M.empty s

-- | Transform a routing table into a distance vector for a given recipient
-- | Ignores entries pointing to the recipient
dv :: Destination -> RTable -> DV
dv d = M.filterWithKey (\d' _ -> d' /= d) . M.map fst

-- | Generalise over functions that create distance vectors
type GetDV = Source -> Destination -> State -> DV

-- | Without split-horizon
woSH :: GetDV
woSH s d = dv d . rtable s

-- | With split-horizon: omit destinations that have the recipient as gateway
wSH :: GetDV
wSH s d = dv d . M.filter ((/= d) . snd) . rtable s

-- | Generate all advertisement messages
send :: GetDV -> State -> Network -> Ads
send f st = M.mapWithKey (\s -> M.mapWithKey (\d _ -> f s d st))

{------------}
{- UPDATING -}
{------------}

-- | Merge two routing tables, selecting those entries with less cost
merge :: RTable -> RTable -> RTable
merge = M.unionWith min

-- | Process an ad sent by the given gateway, with the given distance vector,
-- | into the current routing table
update :: Gateway -> DV -> RTable -> RTable
update g dv rtable = case M.lookup g rtable of
                       -- This node has no such gateway, the ad is wrong
                       Nothing -> rtable
                       -- Get the cost to the gateway and add it to the DV
                       Just (c, _) -> let rtable' = M.map ((, g) . (+ c)) dv
                                       in merge rtable rtable'

-- | Update a given routing table with all the adds addressed to it
updateNode :: M.Map Source DV -> RTable -> RTable
updateNode ads rtable = M.foldWithKey update rtable ads

-- | Update all routing tables with the ads sent to them
receive :: Ads -> State -> State
receive ads = M.mapWithKey (\s -> updateNode (M.findWithDefault M.empty s (swap ads)))

{-----------}
{- RUNNING -}
{-----------}

-- | Run for a single tick
tick :: GetDV -> Network -> State -> (State, Ads)
tick f net st = let adsSent = send f st net
                 in (receive adsSent st, adsSent)

-- | Run until the state is stable
run :: GetDV -> Network -> State -> [(State, Ads)]
run f net st = let (st', ads) = tick f net st
                in (st, ads) : if st == st' then [] else run f net st'

-- | Run for n steps or until the state is stable
runN :: GetDV -> Network -> Int -> State -> [(State, Ads)]
runN f net n = take n . run f net

{------------}
{- PRINTING -}
{------------}

printNetwork :: Network -> String
printNetwork = unlines
             . map (\(s, d, c) -> printf "<%s, %s, %d>" s d c)
             . flatten

printState :: State -> String
printState = unlines
           . map (\(s, d, (c, g)) -> printf "<%s, %s, %d, %s>" s d c g)
           . flatten

printAds :: Ads -> String
printAds = unlines
         . map (\(s, d, dv) -> printf "%s sends %s DV %s" s d (show dv))
         . flatten

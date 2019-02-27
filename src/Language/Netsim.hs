{-# LANGUAGE DeriveGeneric, TupleSections #-}

module Language.Netsim where

import qualified Data.HashMap as M
import Data.Maybe (fromMaybe)
import Data.Hashable (Hashable)
import Text.Printf (printf)

{-----------------}
{- TYPE SYNONYMS -}
{-----------------}

type Node = String
type Source = Node
type Destination = Node
type Gateway = Node
type Cost = Int

type DI a b c = M.Map a (M.Map b c)

-- Static
type Network = DI Source Destination Cost

-- Dynamic
type RTable = M.Map Destination (Cost, Gateway)
type State = DI Source Destination (Cost, Gateway)

-- Messages
type DV = M.Map Destination Cost

swap :: (Hashable a, Ord a, Hashable b, Ord b) => DI a b c -> DI b a c
swap = M.unionsWith M.union
     . M.elems
     . M.mapWithKey (\n -> M.map (\a -> M.singleton n a))


{--------------------------}
{- STRUCTURAL COMPUTATION -}
{--------------------------}

flatten :: DI a b c -> [(a, b, c)]
flatten = concatMap (M.elems) . M.elems . M.mapWithKey (\a -> M.mapWithKey (\b c -> (a, b, c)))

unflatten :: (Hashable a, Ord a, Hashable b, Ord b) => [(a, b, c)] -> DI a b c
unflatten = foldr (\(a, b, c) -> M.insertWith M.union a (M.singleton b c)) M.empty

bidir :: (Hashable a, Ord a) => DI a a b -> DI a a b
bidir net = M.unionWith M.union net (swap net)

ego :: Network -> Network
ego = M.mapWithKey (\n -> M.insert n 0)

network :: [(Node, Node, Cost)] -> Network
network = ego . bidir . unflatten

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

{------------------}
{- INITIALIZATION -}
{------------------}

-- Initial state: each node knows about its neighbours
state :: Network -> State
state = M.map (M.mapWithKey (\d c -> (c, d)))

{---------------}
{- ADVERTISING -}
{---------------}

type GetDV = Source -> Destination -> State -> DV

-- Without split-horizon: no extra checks on the DVs sent out
woSH :: GetDV
woSH s _ = M.map fst . M.findWithDefault M.empty s

-- With split-horizon: do not include d in the distance vector sent to d
wSH :: GetDV
wSH s d = M.filterWithKey (\d' c -> d' /= d) . woSH s d

send :: GetDV -> State -> Network -> DI Source Destination DV
send f st = M.mapWithKey (\s -> M.mapWithKey (\d _ -> f s d st))

{------------}
{- UPDATING -}
{------------}

merge :: RTable -> RTable -> RTable
merge = M.unionWith min

update :: Gateway -> DV -> RTable -> RTable
update g dv rtable = case M.lookup g rtable of
                       Nothing -> rtable
                       Just (c, _) -> let rtable' = M.map ((, g) . (+ c)) dv
                                       in merge rtable rtable'

updateNode :: M.Map Source DV -> RTable -> RTable
updateNode ads rtable = M.foldWithKey update rtable ads

receive :: DI Destination Source DV -> State -> State
receive ads = M.mapWithKey (\s -> updateNode (M.findWithDefault M.empty s ads))

{-----------}
{- RUNNING -}
{-----------}

tick :: GetDV -> Network -> State -> State
tick f net st = let adsSent = send f st net
                 in receive (swap adsSent) st

run :: GetDV -> Network -> State -> [State]
run f net st = let st' = tick f net st
                in st : if st == st'
                           then []
                           else run f net st'

runN :: GetDV -> Network -> Int -> State -> [State]
runN f net n = take n . run f net

{------------}
{- EXAMPLES -}
{------------}

ex1 :: Network
ex1 = network
    [ ("alpha", "beta", 0)
    , ("gamma", "beta", 1)
    ]

ex2 :: Network
ex2 = network
    [ ("n1", "n3", 6)
    , ("n3", "n6", 1)
    , ("n6", "n5", 2)
    , ("n5", "n4", 1)
    , ("n4", "n2", 1)
    , ("n2", "n1", 3)
    , ("n2", "n5", 3)
    , ("n1", "n5", 1)
    , ("n5", "n3", 3)
    ]

ex3 :: Network
ex3 = network
    [ ("A", "B", 3)
    , ("A", "C", 23)
    , ("B", "C", 2)
    , ("C", "D", 5)
    ]

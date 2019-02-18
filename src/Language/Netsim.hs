module Language.Netsim where

import qualified Data.HashMap as M
import Data.Maybe (mapMaybe)
import Data.List (nub)

type Node = String
type Distance = Int

-- Link: ((N1, N2), Distance), where N1 < N2 lexicographically
type Link = ((Node, Node), Distance)

-- Network links
type Network = [Link]

-- Routing table: Destination -> (Distance, Gateway)
type RTable = M.Map Node (Distance, Node)

-- Network state: Node -> Routing table
type State = M.Map Node RTable

-- Directed advertisement: Destination -> Routing table
type Advertisement = (Node, RTable)

-- For a given node and link, get destination and distance, if any
nodeLink :: Node -> Link -> Maybe (Node, Distance)
nodeLink n ((n1, n2), d)
  | n == n1 = Just (n2, d)
  | n == n2 = Just (n1, d)
  | otherwise = Nothing

-- Get a routing table with just links to neighbours
neighbourTable :: Node -> Network -> RTable
neighbourTable n1
  = M.fromList
  . map (\(n2, d) -> (n2, (d, n1))) -- TODO: use arrows
  . mapMaybe (nodeLink n1)

-- Union of the rtable to neighbours and the rtable build so far
rtable :: Node -> Network -> State -> RTable
rtable n net st = neighbourTable n net `M.union` M.findWithDefault M.empty n st

-- Neighbour nodes for a given node
neighbours :: Node -> Network -> [Node]
neighbours n = M.keys . neighbourTable n

-- Advertisement messages for a given node
nodeAds :: Node -> Network -> State -> [Advertisement]
nodeAds n net st = map (\n' -> (n', rtable n' net st)) (neighbours n net)

-- All nodes in a network
nodes :: Network -> [Node]
nodes = nub . concatMap (\((n1, n2), _) -> [n1, n2])

-- All advertisement messages in the network
netAds :: Network -> State -> [Advertisement]
netAds net st = concatMap (\n -> nodeAds n net st) (nodes net)

rtableUpdate :: RTable -> [RTable] -> RTable
rtableUpdate = undefined

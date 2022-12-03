----------------------------
-- ADVENT OF CODE: DAY 12 --
----------------------------
import System.IO
import Data.Char
import Data.List
import Data.Matrix
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

-- Auxiliary functions
--------------------------_
splitBy :: String -> String -> [String]
splitBy _ "" = []
splitBy sep str = case findIndex (isPrefixOf sep) (tails str) of
                    Just idx -> take idx str : splitBy sep (drop (idx+length sep) str)
                    Nothing -> [str | not (null str)]
--------------------------'

-- Data types
--------------------------_
data Node = Start | End | Node String
    deriving (Eq,Show)
type Edge = (Node, Node)
type AdjList = [Node]
type Graph = Map Node AdjList

instance Ord Node where
    compare Start Start = EQ
    compare End End = EQ
    compare Start _ = LT
    compare _ Start = GT
    compare End _   = GT
    compare _ End   = LT
    compare (Node a) (Node b) = a `compare` b

readNode :: String -> Node
readNode "start" = Start
readNode "end" = End
readNode str = Node str

readGraph :: [String] -> Graph
readGraph = foldl (flip addEdge) Map.empty . map readEdge

addEdge :: Edge -> Graph -> Graph
addEdge (a,b) = Map.insertWith (++) a [b] . Map.insertWith (++) b [a]

readEdge :: String -> Edge
readEdge str = (a,b)
    where [a,b] = map readNode $ splitBy "-" str

isReusable :: Node -> Bool
isReusable (Node a) = all isUpper a
isReusable _ = False
--------------------------'

-- PART 1
--------------------------_

pathsFrom :: Node -> Graph -> Int -- TODO: Consider used nodes or not
pathsFrom End _ = 1
pathsFrom node g = sum . map (\ neigh -> pathsFrom' neigh g $ Set.singleton node) $ g Map.! node
    where pathsFrom' End _ _ = 1
          pathsFrom' node graph seen = sum . map (\ neigh -> pathsFrom' neigh g (node `Set.insert` seen)) . filter (not . (`Set.member` seen)) . filter isReusable $ graph Map.! node

solve1 :: Graph -> Int
solve1 = pathsFrom Start
--------------------------'

-- PART 2
--------------------------_
--------------------------'

parseInputs :: String -> Graph
parseInputs = readGraph . filter (/="") . lines

ioinputs :: IO Graph
ioinputs = parseInputs <$> readFile "inputs"

main :: IO ()
main = withFile "inputs" ReadMode (\ handle -> do
    contents <- hGetContents handle
    let inputs = parseInputs contents
    let result1 = solve1 inputs
    -- let result2 = solve2 inputs
    putStrLn $ "Part 1: " ++ show result1
    -- putStrLn $ "Part 2: " ++ show result2
    putStrLn "Done"
                                  )

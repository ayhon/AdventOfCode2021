---------------------------
-- ADVENT OF CODE: DAY 9 --
---------------------------
import System.IO
import Data.Char
import Data.List
import Data.Matrix
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

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
type Depth = Int
data Point = Point {
    getPos :: (Int, Int),
    getDepth :: Depth
                   } deriving Show
type DepthMatrix = Matrix Int

getNeighbours :: Int -> Int -> DepthMatrix -> [Point]
getNeighbours x0 y0 m = [Point (x,y) depth | x <- [x0-1..x0+1], y <- [y0-1..y0+1], (x == x0) == (y /= y0), depth <- catMaybes [safeGet x y m]]

getLowPoints :: DepthMatrix -> [Point]
getLowPoints m = catMaybes . toList $ mapPos (\ (x,y) h -> if isMinimum x y m then Just (Point (x,y) h) else Nothing) m

isMinimum :: Int -> Int -> DepthMatrix -> Bool
isMinimum x y m = getElem x y m < minimum (map getDepth $ getNeighbours x y m)

getBasins :: DepthMatrix -> [Set (Int, Int)]
getBasins m = map (expandBasin Set.empty) $ getLowPoints m
    where expandBasin seen (Point (x,y) 9) = seen
          expandBasin seen (Point (x,y) h) = if Set.member (x,y) seen 
                                  then seen 
                                  else growFrom x y (Set.insert (x,y) seen)
          growFrom x y seen = foldl expandBasin seen $ getNeighbours x y m
--------------------------'

-- PART 1
--------------------------_
solve1 :: DepthMatrix -> Int
solve1 m = sum $ mapPos getRiskLevelIfMinimum m
    where getRiskLevelIfMinimum (x,y) height = if isMinimum x y m 
                                                  then height + 1 
                                                  else 0
--------------------------'

-- PART 2
--------------------------_
solve2 :: DepthMatrix -> Int
solve2 = product . take 3 . reverse . sort . map Set.size . getBasins
--------------------------'

parseInputs :: [String] -> DepthMatrix
parseInputs = fromLists . map readRow
    where readRow row = map (\ c -> read [c]) row

ioinputs :: IO DepthMatrix
ioinputs = parseInputs . filter (/="") . lines <$> readFile "inputs"

main :: IO ()
main = withFile "inputs" ReadMode (\ handle -> do
    contents <- hGetContents handle
    let inputs = parseInputs $ filter (/="") $ lines contents
    let result1 = solve1 inputs
    let result2 = solve2 inputs
    putStrLn $ "Part 1: " ++ show result1
    putStrLn $ "Part 2: " ++ show result2
    putStrLn "Done"
                                  )

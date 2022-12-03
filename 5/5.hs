---------------------------
-- ADVENT OF CODE: DAY 5 --
---------------------------
import System.IO
import Data.Char
import Data.List
import Control.Applicative (ZipList)
import qualified Data.MultiSet as MS
import Debug.Trace

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
data Point = Point Int Int
    deriving (Eq, Ord)
data Line = Line Point Point

instance Show Point where
    show (Point x y) = show x++","++show y
instance Show Line where
    show (Line beg end) = show beg++" -> "++show end

readPoint :: String -> Point
readPoint str = Point x y
    where [x,y] = map read $ splitBy "," str

readLine :: String -> Line
readLine str = Line start end
    where [s,e] = map readPoint $ splitBy "->" str
          start = min s e
          end = max s e

isDiagonal :: Line -> Bool
isDiagonal (Line (Point x1 y1) (Point x2 y2)) = not (x1 == x2 || y1 == y2)
--------------------------'

-- PART 1
--------------------------_
towards :: Int -> Int -> [Int]
towards x y
  | x < y = take len [x..]
  | x > y = take len [x,x-1..]
  | otherwise = repeat x
    where len = abs (x-y) + 1

addLinePoints :: Line -> MS.MultiSet Point -> MS.MultiSet Point
addLinePoints (Line (Point x1 y1) (Point x2 y2)) ms = foldr MS.insert ms line_points
    where line_points = zipWith Point (x1 `towards` x2) (y1 `towards` y2)

getIntersections :: [Line] -> MS.MultiSet Point
getIntersections = foldr addLinePoints MS.empty

solve1 :: [Line] -> Int
solve1 = solve2 . filter (not . isDiagonal)
--------------------------'

-- PART 2
--------------------------_
solve2 :: [Line] -> Int
solve2 ls = result
    where traces = getIntersections ls
          repeated = MS.foldOccur (updateIfMoreThan 1) MS.empty traces
          updateIfMoreThan limit elem occur res = if occur > limit then MS.insert elem res else res
          result = MS.distinctSize repeated
--------------------------'

main :: IO ()
main = withFile "inputs" ReadMode (\ handle -> do
    contents <- hGetContents handle
    let inputs = map readLine . filter (/="") $ lines contents
    let result1 = solve1 inputs
    let result2 = solve2 inputs
    putStrLn $ "Part 1: " ++ show result1
    putStrLn $ "Part 2: " ++ show result2
    putStrLn "Done"
                                  )

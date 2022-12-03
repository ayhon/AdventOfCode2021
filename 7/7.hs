---------------------------
-- ADVENT OF CODE: DAY 7 --
---------------------------
import System.IO
import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace

-- Auxiliary functions
--------------------------_
splitBy :: String -> String -> [String]
splitBy _ "" = []
splitBy sep str = case findIndex (isPrefixOf sep) (tails str) of
                    Just idx -> take idx str : splitBy sep (drop (idx+length sep) str)
                    Nothing -> [str | not (null str)]

loadCommaList :: String -> [Int]
loadCommaList = map read . splitBy ","
--------------------------'

-- Data types
--------------------------_
data ListLocation = Range Int Int | Position Int | Nowhere

range :: Int -> Int -> ListLocation
range x y
 | x == y = Position x
 | x < y = Range x y
 | otherwise = Nowhere

size ::  ListLocation -> Int
size (Range b e) = e-b+1
size (Position _) = 1
size Nowhere = 0

instance Show ListLocation where
    show (Range b e ) = "<"++show b++"->"++show e++">"
    show (Position i) = "<"++show i++">"
    show  Nowhere     = "<>"
--------------------------'

-- PART 1
--------------------------_
costTo :: Int -> [Int] -> Int
costTo n = foldl (\ acc x -> acc + abs (x-n)) 0

-- TODO: Fix this f-ing function
-- Probably the invariants are wrong
searchMinimum' :: Ord a => Show a => ListLocation -> [a] -> Int
searchMinimum' Nowhere _ = error "Wait, wtf happened"
searchMinimum' (Position i) ls = i
searchMinimum' r@(Range b e) ls
  | size r == 2               = error "how?"
  | size r == 3               = q 2
  | vq 3 > max (vq 2) (vq 4) = searchMinimum' (range (q 2) (q 4)) ls
  | vq 1 > max (vq 2) (vq 0) = searchMinimum' (range (q 0) (q 2)) ls
  | otherwise                = searchMinimum' (range (q 1) (q 3)) ls
  where q x = b + x*(e-b) `div` 4
        vq = (ls!!) . q

searchMinimum :: Ord a => Show a => [a]-> Int
searchMinimum ls = searchMinimum' (Range 0 $ length ls - 1) ls

solve1 :: [Int] -> Int
solve1 crabs = costTo result crabs
    where n = length crabs
          result = crabs !! searchMinimum (map (`costTo` crabs) [0..n])
--------------------------'

-- PART 2
--------------------------_
sumFromTo :: Int -> Int -> Int
sumFromTo b e = (abs (e-b) +1)*abs (e-b) `div` 2

costTo' :: Int -> [Int] -> Int
costTo' n = foldl (\ acc x -> acc + sumFromTo x n) 0

binSearch :: [Int] -> (Int -> Bool) -> Int
binSearch [] _ = 0
binSearch [x] p = if p x then 1 else 0
binSearch ls p
  | p(ls !! m) = length lhs + binSearch rhs p
  | otherwise = binSearch lhs p
  where m = length ls `div` 2
        (lhs,rhs) = splitAt m ls

-- TODO: This doesn't work. I had to solve it manually from the command line
solve2 :: [Int] -> Int
solve2 crabs = costTo' result crabs
    where n = length crabs
          result = crabs !! binSearch (trace (show $ take 15 differences) differences) (<0)
          differences = map (\ x -> costTo' (x+1) crabs - costTo' x crabs ) [0..last crabs]
--------------------------'

iocrabs :: IO [Int]
iocrabs = loadCommaList <$> readFile "inputs"

main :: IO ()
main = withFile "inputs" ReadMode (\ handle -> do
    contents <- hGetContents handle
    let inputs = loadCommaList contents
    let result1 = solve1 $ sort inputs
    let result2 = solve2 $ sort inputs
    putStrLn $ "Part 1: " ++ show result1
    putStrLn $ "Part 2: " ++ show result2
    putStrLn "Done"
                                  )

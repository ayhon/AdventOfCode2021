---------------------------
-- ADVENT OF CODE: DAY 8 --
---------------------------
import System.IO
import Data.Char
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative (ZipList)

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
type SevenSegmentDisplay = Set Char
type Possibilities = [SevenSegmentDisplay]
data Problem = Problem { getPossibilities :: Possibilities
                       , getCurrentDisplay :: [SevenSegmentDisplay]
                       } deriving Show

readProblem :: String -> Problem
readProblem line = Problem possibilities display
    where [possibilities, display] = map readSpaceSeparatedDisplays $ splitBy "|" line

readSpaceSeparatedDisplays :: String -> Possibilities
readSpaceSeparatedDisplays = map readDisplay . filter (/="") . splitBy " "

readDisplay :: String -> SevenSegmentDisplay
readDisplay = Set.fromList . filter (/=' ')

getDisplayDigits :: Problem -> [Int]
getDisplayDigits (Problem poss disp)= map (representation poss Map.!) disp

getDisplayValue :: Problem -> Int
getDisplayValue = foldl (\acc x -> acc*10 + x) 0 . getDisplayDigits

representation :: Possibilities -> Map SevenSegmentDisplay Int
representation poss = foldl (\ acc x -> Map.insert (get x) x acc) Map.empty [1,4,7,8,3,5,2,6,9,0]
    where get 1 = head $ filter ((==2) . Set.size) poss
          get 4 = head $ filter ((==4) . Set.size) poss
          get 7 = head $ filter ((==3) . Set.size) poss
          get 8 = head $ filter ((==7) . Set.size) poss
          -- 1,4 7 and 8 are all unique

          get 3 = head $ filter (\ p -> Set.size (Set.intersection p (get 1)) == 2 && Set.size p == 5) poss
          get 5 = head $ filter (\ p -> p `Set.isSubsetOf` get 6 && Set.size p == 5) poss
          get 2 = head $ filter (\ p -> p /= get 3 && p /= get 5 && Set.size p == 5) poss
          -- 5, 3 and 2 have the same number of segments (5), where
          --   3 and 1 share 2 segments
          --   5 is contained in 6
          --   2 isn't contained in 6 and shares 1 segment with 1

          get 6 = head $ filter (\ p -> Set.size (Set.intersection p (get 7)) == 2 && Set.size p == 6) poss
          get 9 = head $ filter (\ p -> get 3 `Set.isSubsetOf` p && Set.size p == 6) poss
          get 0 = head $ filter (\ p -> p /= get 6 && p /= get 9 && Set.size p == 6) poss
          -- 0, 9 and 6 have the same number of segments (6), where
          --   6 and 7 share 2 segments
          --   3 is contained in 9
          --   0 isn't contained in 9 and shares 3 segments with 7

          get e = error ("There is no digit "++show e)
--------------------------'

-- PART 1
--------------------------_
solve1 :: [Problem] -> Int
solve1 = sum . map (numOf [1,4,7,8])
  where numOf digs = length . filter (`elem` digs) . getDisplayDigits
--------------------------'

-- PART 2
--------------------------_
solve2 :: [Problem] -> Int
solve2 = sum . map getDisplayValue
--------------------------'

parseInputs :: [String] -> [Problem]
parseInputs = map readProblem

ioinputs :: IO [Problem]
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

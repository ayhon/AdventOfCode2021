---------------------------
-- ADVENT OF CODE: DAY 6 --
---------------------------
import System.IO
import Data.Char
import Data.List
import Data.Maybe
import Control.Applicative (ZipList)
import qualified Data.Map as M
import Data.Map ((!), fromList, empty, Map, update, findWithDefault)
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
type CyclePosition = Int
type NumberOfFish = Int
type Day = Map CyclePosition NumberOfFish

type Cycle = [NumberOfFish]
data State = State {
    getDay :: Int,
    getMainCycle :: Cycle,
    getNewbornsCycle :: Cycle
                   }
instance Show State where
    show (State day main_cycle newborns_cycle) = "Day "++show day++": "++auxShow 0 (main_cycle,newborns_cycle)
        where auxShow idx (x:xs,newborns_cycle) = show idx++"(" ++show x++") "++auxShow (idx+1) (xs,newborns_cycle)
              auxShow idx ([],x:xs) = show idx++"("++show x++") "++auxShow (idx+1) ([],xs)
              auxShow idx ([],[]) = ""
--------------------------'

-- PART 1
--------------------------_
fillUntil :: Int -> [Int] -> [Int]
fillUntil n acc = replicate (n - length acc) 0 ++ acc

loadByCycles :: [[Int]] -> Cycle
loadByCycles ls = result
    where intermediary = foldl (\ acc bank@(cycle:_) -> length bank : fillUntil cycle acc) [] ls
          result = reverse $ fillUntil 7 intermediary

numFish :: State -> NumberOfFish
numFish (State _ main_cycle newborns_cycle) = sum main_cycle + sum newborns_cycle

nextState :: State -> State
nextState (State d main_cycle newborns_cycle) = result
    where new_main_cycle = tail main_cycle ++ [head main_cycle + head newborns_cycle]
          new_newborns_cycle = tail newborns_cycle ++ [head main_cycle]
          result = State (d+1) new_main_cycle new_newborns_cycle

solve1 :: [NumberOfFish] -> Int
solve1 initial_fish = result
    where initial_newborns = replicate 2 0
          initial_state = State 0 initial_fish initial_newborns
          simulation = initial_state : map nextState simulation
          result = numFish $ simulation !! 80
--------------------------'

-- PART 2
--------------------------_
solve2 :: [NumberOfFish] -> Int
solve2 initial_fish = result
    where initial_newborns = replicate 2 0
          initial_state = State 0 initial_fish initial_newborns
          simulation = initial_state : map nextState simulation
          result = numFish $ simulation !! 256

--------------------------'

main :: IO ()
main = withFile "inputs" ReadMode (\ handle -> do
    contents <- hGetContents handle
    let inputs =  group $ sort $ map read $ splitBy "," contents
    let result1 = solve1 $ loadByCycles inputs
    let result2 = solve2 $ loadByCycles inputs
    putStrLn $ "Part 1: " ++ show result1
    putStrLn $ "Part 2: " ++ show result2
    putStrLn "Done"
                                  )

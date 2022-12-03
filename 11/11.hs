----------------------------
-- ADVENT OF CODE: DAY 11 --
----------------------------
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
type Snapshot = Matrix Int
data Cell = Cell { getPos :: (Int,Int)
                 , getValue :: Int
                 } deriving Show

getNeighbours :: (Int, Int) -> Matrix Int -> [Cell]
getNeighbours (x0,y0) m = [ Cell (x,y) v | x <- [x0-1..x0+1],
                                           y <- [y0-1..y0+1],
                                           (x /= x0) || (y /= y0),
                                           v <- catMaybes [safeGet x y m]]

nextState :: Snapshot -> Snapshot
nextState curr_state = resetFlashed . foldl flash updated_state $ getExplodingCells updated_state
    where updated_state = increaseByOne curr_state
          increaseByOne = mapPos (\ _ v -> v+1)
          getExplodingCells = catMaybes . toList . mapPos (\ (x,y) v -> if v == 10 then Just (Cell (x,y) v) else Nothing)
          resetFlashed state = mapPos (\ (x,y) v -> if v >= 10 then 0 else v) state
          flash curr_state (Cell (x,y) v) = foldl flash after_flashes . filter ((==10) . getValue) $ getNeighbours (x,y) after_flashes
              where after_flashes = foldl (
                        \ updating_state (Cell (x,y) v) -> setElem (v+1) (x,y) updating_state
                                            ) curr_state $ getNeighbours (x,y) curr_state

getNumberOfFlashes :: Snapshot -> Int
getNumberOfFlashes = length . filter (==0) . toList
--------------------------'

-- PART 1
--------------------------_
solve1 :: Snapshot -> Int
solve1 initial_state = sum . map getNumberOfFlashes $ take 101 states
    where states = initial_state : map nextState states
--------------------------'

-- PART 2
--------------------------_
solve2 :: Snapshot -> Int
solve2 initial_state = length $ takeWhile ((/= 100) . getNumberOfFlashes) states
    where states = initial_state : map nextState states
--------------------------'

parseInputs :: [String] -> Snapshot
parseInputs = fromLists . map readRow
    where readRow row = map (\ c -> read [c]) row

ioinputs :: IO (Matrix Int)
ioinputs = parseInputs . filter (/="") . lines <$> readFile "inputs"

main :: IO ()
main = withFile "inputs" ReadMode (\ handle -> do
    contents <- hGetContents handle
    let inputs = parseInputs . filter (/="") $ lines contents
    let result1 = solve1 inputs
    let result2 = solve2 inputs
    putStrLn $ "Part 1: " ++ show result1
    putStrLn $ "Part 2: " ++ show result2
    putStrLn "Done"
                                  )

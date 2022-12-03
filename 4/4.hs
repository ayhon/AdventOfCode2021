---------------------------
-- ADVENT OF CODE: DAY 4 --
---------------------------
import System.IO
import Data.Char
import Data.List
import Debug.Trace

-- PART 1
--------------------------_
type Matrix = [[Int]]
data BingoBoard = BingoBoard { getMatrix :: Matrix, getRows :: [Int], getCols :: [Int], sumUnmarked :: Int }
    deriving Show

newBingoBoard :: Matrix -> BingoBoard
newBingoBoard m = BingoBoard m rows cols unmarked
    where cols = replicate side 0
          rows = cols
          side = length m
          unmarked = sum $ map sum m

getElem :: Matrix -> Int -> Maybe (Int, Int)
getElem m elem = result
    where x = findIndex (/= Nothing) $ map (elemIndex elem) m
          y = case x of
                Just a -> elemIndex elem $ m !! a
                Nothing -> Nothing
          result = case (x,y) of
                     (Just a, Just b) -> Just (a,b)
                     (_,_) -> Nothing

said :: Int -> BingoBoard -> BingoBoard
said n (BingoBoard m rows cols unmarked) = case getElem m n of
                              Just (x,y) -> BingoBoard m (update x rows) (update y cols) (unmarked - n)
                                  where update 0 (x:xs) = (x+1):xs
                                        update _ [] = []
                                        update n (x:xs) = x : update (n-1) xs
                              Nothing  -> BingoBoard m rows cols unmarked

simulateUntilBoard :: (BingoBoard -> Bool) -> [Int] -> [BingoBoard] -> (Int, BingoBoard)
simulateUntilBoard isInteresting (num:rest) bingo_boards = result
    where updated_bingo_boards = map (said num) bingo_boards
          interesting_boards = filter isInteresting updated_bingo_boards
          result = if length interesting_boards /= 1
                    then simulateUntilBoard isInteresting rest updated_bingo_boards
                    else (num, head interesting_boards)
simulateUntilBoard _ _ _ = (0, newBingoBoard (replicate 5 $ replicate 5 0))

isWinnerBoard :: BingoBoard -> Bool
isWinnerBoard (BingoBoard m rows cols _) = result
    where result = any condition rows || any condition cols
          condition = \ x -> x == length m

solve1 :: [Int] -> [BingoBoard] -> Int
solve1 numbers bingo_boards = last_number * sumUnmarked winner_board
    where (last_number,winner_board) = simulateUntilBoard isWinnerBoard numbers bingo_boards
--------------------------'

-- PART 2
--------------------------_
solve2 :: [Int] -> [BingoBoard] -> Int
solve2 numbers bingo_boards = last_number * sumUnmarked marked_loser_board
    where (dooming_number, loser_board) = simulateUntilBoard (not . isWinnerBoard) numbers bingo_boards
          (last_number, marked_loser_board) = simulateUntilBoard isWinnerBoard (dropWhile (/=dooming_number) numbers) [loser_board]
--------------------------'

split :: Eq a => a -> [a] -> [[a]]
split sep ls = case elemIndex sep ls of
                  Just i -> group : split sep rest
                    where (group,_:rest) = splitAt i ls
                  Nothing -> [ ls | not (null ls)]

groupBy5 :: [a] -> [[a]]
groupBy5 [] = []
groupBy5 ls = group : groupBy5 rest where (group,rest) = splitAt 5 ls

readInt :: String  -> Int
readInt = read

main :: IO()
main = withFile "inputs" ReadMode (\ handle -> do
    contents <- hGetContents handle
    let inputs = filter (/="") $ split '\n' contents
    let numbers = map read $ split ',' $ head inputs :: [Int]
    let matrices = groupBy5 . map (map read . filter (/="") . split ' ') $ tail inputs :: [Matrix]
    let bingo_boards = map newBingoBoard matrices :: [BingoBoard]
    let result1 = solve1 numbers bingo_boards
    let result2 = solve2 numbers bingo_boards
    putStrLn $ "Part 1: " ++ show result1
    putStrLn $ "Part 2: " ++ show result2
    putStrLn "Done"
                                  )

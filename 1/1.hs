---------------------------
-- ADVENT OF CODE: DAY 1 --
---------------------------
import System.IO

-- PART 1
--------------------------_
solve1 :: [Int] -> Int
solve1 [] = 0
solve1 (x:[]) = 0
solve1 (x:y:xs) = (if x < y
                    then 1
                    else 0) + solve1(y:xs)
--------------------------'

-- PART 2
-------------------------_
sliding_window :: [Int] -> [(Int, Int, Int)]
sliding_window (x:y:z:rest) = (x,y,z):(sliding_window $ y:z:rest)
sliding_window _ = []

solve2 :: [Int] -> Int
solve2 = solve1 . map (\ (x,y,z) -> x + y + z) . sliding_window
--------------------------'

main :: IO()
main = withFile "inputs" ReadMode $ (\ handle -> do
    contents <- hGetContents handle
    let inputs = map read $ words contents
    let result1 = solve1 inputs
    let result2 = solve2 inputs
    putStrLn $ concat ["Part 1: ", show result1]
    putStrLn $ concat ["Part 2: ", show result2]
                                    )

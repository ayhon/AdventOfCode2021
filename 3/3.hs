---------------------------
-- ADVENT OF CODE: DAY 3 --
---------------------------
import System.IO
import Data.Char
import Data.List
import Debug.Trace

-- PART 1
--------------------------_
type Bit = Int
type Byte = [Bit]

toBit :: Char -> Bit
toBit a = read [a] :: Bit

toByte :: String -> Byte
toByte = map toBit

addBytes :: Byte -> Byte -> Byte
addBytes (x:xs) (y:ys) = (x+y) : addBytes xs ys
addBytes _ _ = []

toInt :: Byte -> Int
toInt = foldl (\ acc x -> acc*2 + x) 0

solve1 :: [Byte] -> Int
solve1 bytes = gamma * epsilon
    where byte_absolute_frequencies = foldl addBytes (repeat 0) bytes
          threshold = length bytes `div` 2
          gamma = toInt [if x > threshold then 1 else 0 | x <- byte_absolute_frequencies]
          epsilon = toInt [if x < threshold then 1 else 0 | x <- byte_absolute_frequencies]
--------------------------'

-- PART 2
------------------------_
-- getO2GeneratorRating :: [Byte] -> Byte
-- getO2GeneratorRating [byte] = byte
-- getO2GeneratorRating bytes = getO2GeneratorRating 
--     . map tail
--     $ filter (\ (bit:rest) -> bit == most_common_bit) bytes
--         where bits_on = sum $ map head bytes :: Bit
--               most_common_bit = if bits_on > length bytes `div` 2 then 1 else 0

-- getCO2ScrubberRating :: [Byte] -> Byte
-- getCO2ScrubberRating [byte] = byte
-- getCO2ScrubberRating bytes = getCO2ScrubberRating 
--     . map tail
--     $ filter (\ (bit:rest) -> bit == least_common_bit) bytes
--         where bits_on = sum $ map head bytes :: Bit
--               least_common_bit = if bits_on < length bytes `div` 2 then 1 else 0

filteringProcess :: ([Byte] -> Byte -> Bool) -> [Byte] -> Byte
filteringProcess _ [byte] = byte
filteringProcess criteria bytes = trace (show bytes) result
    where valid = filter (criteria bytes) bytes
          end_of_result = filteringProcess criteria $ map tail valid
          first_bit = head $ head valid
          result = first_bit: end_of_result

leastCommon :: Ord a => [a] -> a
leastCommon = head . head . sortOn length . group . sort

mostCommon :: Ord a => [a] -> a
mostCommon = head . last . sortOn length . group . sort

getO2GeneratorRating :: [Byte] -> Byte
getO2GeneratorRating = filteringProcess criteria
    where criteria bytes (bit:_) = bit == mostCommon (map head bytes)

getCO2ScrubberRating :: [Byte] -> Byte
getCO2ScrubberRating = filteringProcess criteria
    where criteria bytes (bit:_) = bit == leastCommon (map head bytes)

solve2 :: [Byte] -> Int
solve2 bytes = o2_generator_rating * co2_scrubber_ratting
    where o2_generator_rating = toInt $ getO2GeneratorRating bytes
          co2_scrubber_ratting = toInt $ getCO2ScrubberRating bytes
--------------------------'

main :: IO()
main = withFile "inputs" ReadMode (\ handle -> do
    contents <- hGetContents handle
    let inputs = map toByte
                . filter (/="")
                $ lines contents :: [Byte]
    let result1 = solve1 inputs
    let result2 = solve2 inputs
    putStrLn $ "Part 1: " ++ show result1
    putStrLn $ "Part 2: " ++ show result2
                                  )








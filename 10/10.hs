----------------------------
-- ADVENT OF CODE: DAY 10 --
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
--------------------------'

-- PART 1
--------------------------_
syntaxCheckerScore :: Char -> Int
syntaxCheckerScore ')' = 3
syntaxCheckerScore ']' = 57
syntaxCheckerScore '}' = 1197
syntaxCheckerScore '>' = 25137
syntaxCheckerScore _ = 0

closing :: Char -> Char
closing '(' = ')'
closing '[' = ']'
closing '{' = '}'
closing '<' = '>'
closing x = x

firstFail' :: [Char] -> String -> Maybe Char
firstFail' _ "" = Nothing
firstFail' [] (c:cs) = firstFail' [c] cs
firstFail' (top:res) (c:cs)
  | closing top == c = firstFail' res cs
  | c `elem` opens = firstFail' (c:top:res) cs
  | c `elem` closed = Just c
  where opens = ['(','[','{','<']
        closed = map closing opens

firstFail :: String -> Maybe Char
firstFail = firstFail' []

solve1 :: [String] -> Int
solve1 = sum . map syntaxCheckerScore . mapMaybe firstFail
--------------------------'

-- PART 2
--------------------------_
autocompleteScore :: Char -> Int
autocompleteScore ')' = 1
autocompleteScore ']' = 2
autocompleteScore '}' = 3
autocompleteScore '>' = 4
autocompleteScore _ = 0

missingCharacters' :: [Char] -> String -> Maybe [Char]
missingCharacters' stack "" = Just $ map closing stack
missingCharacters' [] (c:cs) = missingCharacters' [c] cs
missingCharacters' stack@(top:res) (c:cs)
  | closing top == c = missingCharacters' res cs
  | c `elem` opens = missingCharacters' (c:stack) cs
  | c `elem` closed = Nothing
  where opens = ['(','[','{','<']
        closed = map closing opens

missingCharacters :: String -> Maybe [Char]
missingCharacters = missingCharacters' []

solve2 :: [String] -> Int
solve2 = takeMiddle . sort . map scoreMissing . mapMaybe missingCharacters
    where scoreMissing = foldl (\ acc x -> acc*5 + x) 0 . map autocompleteScore
          takeMiddle ls = ls !! (length ls `div` 2)
--------------------------'

ioinputs :: IO [String]
ioinputs = filter (/="") . lines <$> readFile "inputs"

main :: IO ()
main = withFile "inputs" ReadMode (\ handle -> do
    contents <- hGetContents handle
    let inputs = filter (/="") $ lines contents
    let result1 = solve1 inputs
    let result2 = solve2 inputs
    putStrLn $ "Part 1: " ++ show result1
    putStrLn $ "Part 2: " ++ show result2
    putStrLn "Done"
                                  )

---------------------------
-- ADVENT OF CODE: DAY 2 --
---------------------------
import System.IO
import Data.Char

-- PART 1
--------------------------_
data Move = Up Int | Down Int | Forward Int
    deriving (Show, Read)
data Position = Position Int Int

addMove :: Position -> Move -> Position
addMove (Position x y) (Forward ox) = Position (x + ox) y
addMove (Position x y) (Down oy) = Position x (y + oy)
addMove (Position x y) (Up oy) = Position x (y - oy)

origin :: Position
origin = Position 0 0

solve1 :: [Move] -> Int
solve1 moves = horizontal * vertical
  where (Position horizontal vertical) = foldl addMove origin moves
--------------------------'

-- PART 2
------------------------_
newtype Aim = Aim Int
data AimedPosition = AimedPosition Aim Position

addMoveWithAim :: AimedPosition -> Move -> AimedPosition
addMoveWithAim (AimedPosition (Aim a) (Position x y)) (Forward disp) = 
    AimedPosition (Aim a) (Position (x+disp) (y + disp*a))
addMoveWithAim (AimedPosition (Aim a) position) (Down oa) = 
    AimedPosition (Aim (a + oa)) position
addMoveWithAim (AimedPosition (Aim a) position) (Up oa) =
    AimedPosition (Aim (a - oa)) position

aimedOrigin :: AimedPosition
aimedOrigin = AimedPosition (Aim 0) origin

solve2 :: [Move] -> Int
solve2 moves = h * v
    where (AimedPosition aim (Position h v)) = foldl addMoveWithAim aimedOrigin moves
--------------------------'

firstUppercase :: [Char] -> [Char]
firstUppercase (first:rest) = toUpper first : rest
firstUppercase x = x

main :: IO()
main = withFile "inputs" ReadMode (\ handle -> do
    contents <- hGetContents handle
    let inputs = map (read . firstUppercase) 
                . filter (/="") 
                $ lines contents :: [Move]
    let result1 = solve1 inputs
    let result2 = solve2 inputs
    putStrLn $ "Part 1: " ++ show result1
    putStrLn $ "Part 2: " ++ show result2
                                  )








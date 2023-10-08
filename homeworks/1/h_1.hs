{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Main
  where
import Control.Monad.RWS (First(getFirst))

-- main=putStrLn "Hello, World!"

type Result = [String]
pp :: Result -> IO ()
pp x = putStr (concat (map (++"\n") x))

-- 1 - Magic 15 Puzzle

-- Implement the function puzzle, it will simulate the game similar to 15 Puzzle. In our case, we have 25 squares, where 24 squares are ocupied by tiles with big case letters from 'A' to 'X'. One tile is free, it is denotated by ' '. In one move, you can move a tile (denotated by its letter) into the free one. The function gets the original configuration and a sequence of valid moves. It will output the resulting configuration. 

-- puzzle2 :: [String]
-- puzzle2 = ["AC DE",
--            "FBHIJ",
--            "KGLNO",
--            "PQMRS",
--            "UVWXT"]

-- puzzle :: Result -> [Char] -> Result


-- Define the puzzle2 data structure
puzzle2 :: [String]
puzzle2 = ["AC DE",
           "FBHIJ",
           "KGLNO",
           "PQMRS",
           "UVWXT"]

-- Function to switch two characters at given coordinates
switchCharacters :: [String] -> (Int, Int) -> (Int, Int) -> [String]
switchCharacters grid (x1, y1) (x2, y2) =
  if validCoordinates x1 y1 && validCoordinates x2 y2
    then switchGridElements grid (x1, y1) (x2, y2)
    else grid

-- Check if the given coordinates are valid for the grid
validCoordinates :: Int -> Int -> Bool
validCoordinates x y =
  x >= 0 && x < length puzzle2 && y >= 0 && y < length (head puzzle2)

-- Switch two elements in the grid at the given coordinates
switchGridElements :: [String] -> (Int, Int) -> (Int, Int) -> [String]
switchGridElements grid (x1, y1) (x2, y2) =
  if x1 == x2 then
    let row1 = grid !! x1
        elem1 = row1 !! y1
        elem2 = row1 !! y2
        newRow1 = replaceAtIndex y1 (replaceAtIndex y2 row1 elem1) elem2
    in replaceAtIndex x1 grid newRow1
  else
    let row1 = grid !! x1
        row2 = grid !! x2
        elem1 = row1 !! y1
        elem2 = row2 !! y2
        newRow1 = replaceAtIndex y1 row1 elem2
        newRow2 = replaceAtIndex y2 row2 elem1
    in 
      replaceAtIndex x1 (replaceAtIndex x2 grid newRow2) newRow1

-- Helper function to replace an element at a specific index in a list
replaceAtIndex :: Int -> [a] -> a -> [a]
replaceAtIndex index xs newElement =
  take index xs ++ [newElement] ++ drop (index + 1) xs

-- Function to apply a function to a value (pipe operator)
(|>) :: a -> (a -> b) -> b
(|>) x f = f x


-- Define a type class for structures that can contain characters
class CharacterContainer a where
  findCharCoordinates :: Char -> a -> [(Int, Int)]
  switchCoordinates :: Char -> Char -> a -> a

-- Implement the CharacterContainer type class for a list of strings
instance CharacterContainer [String] where
  findCharCoordinates char grid =
    [(row, col) | (row, rowString) <- zip [0..] grid, (col, gridChar) <- zip [0..] rowString, gridChar == char]

getFirstItem :: [a] -> a
getFirstItem (x:_) = x
getFirstItem [] = error "Empty list"

-- Function to apply switchCoordinates for each character in a sequence
applySwitchSequence :: String -> [String] -> [String]
applySwitchSequence sequence grid =
  -- foldl (\grid' char -> if char /= ' ' then switchCoordinates char ' ' grid' else grid') grid sequence
  foldl (\grid' char -> if char /= ' ' then switchCharacters grid (getFirstItem (findCharCoordinates ' ' grid')) (getFirstItem(findCharCoordinates char grid')) else grid') grid sequence


main :: IO ()
main = do
  putStrLn "Origin Grid:"
  mapM_ putStrLn puzzle2
  let newGrid = switchCharacters puzzle2 (0, 0) (1, 1)  -- Example: Switch (0,0) and (1,1)
  -- putStrLn "Modified Grid 1:"
  -- mapM_ putStrLn newGrid
  -- let newGrid2 = switchCharacters puzzle2 (0, 2) (1, 2)
  -- putStrLn "Modified Grid 2:"
  -- mapM_ putStrLn newGrid2
  -- let newGrid3 = switchCharacters puzzle2 (0, 2) (0, 1)
  -- putStrLn "Modified Grid 3:"
  -- mapM_ putStrLn newGrid3

  -- let inputSequence = "C"
  -- -- inputSequence2 = "H"
  -- let newGrid4 = applySwitchSequence inputSequence puzzle2
  --     -- newGrid2 = applySwitchSequence inputSequence2 puzzle2
  -- putStrLn "Modified Grid 4:"
  -- mapM_ putStrLn newGrid4

  -- let inputSequence2 = "CBGLMRST"
  let inputSequence2 = "CBG"
  let newGrid5 = applySwitchSequence inputSequence2 puzzle2
      -- newGrid2 = applySwitchSequence inputSequence2 puzzle2
  putStrLn "Modified Grid 5:"
  putStrLn inputSequence2
  mapM_ putStrLn newGrid5

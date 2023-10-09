{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}

-- above are recommended options for VSCode

-- define main
module Main
  where
-- import suggested libraries
import Control.Monad.RWS (First(getFirst))



-- result structure a print, not used
type Result = [String]
pp :: Result -> IO ()
pp x = putStr ( ((replicate (length x+2) '-' ))++ "\n" ++ concat (map ("|"++) (map (++"|\n") x)) ++ ((replicate (length x+2) '-' )) ++ "\n")

-- 7 - Ticktacktoe

-- Implement the function ticktack which has 2 arguments. First argument is a tuple of natural numbers and defines the number of columns and rows of a play field. Coordinates are counted from bottom left corner. Second list contains a record of a match of ticktacktoe game given by coordinates on which played in turns player 'x' and player 'o'. Print actual state of the game in the way where play-field will be bordered by characters '-' and '|', empty squares ' ' and characters 'x' and 'o' will be on squares where the players have played. 

-- ticktack::(Int,Int) -> [(Int,Int)] -> Result

-- Prelude>pp(ticktack (8,8) [(1,1),(8,8),(2,2),(3,3),(4,2),(3,2)])
-- ----------
-- |       o|
-- |        |
-- |        |
-- |        |
-- |        |
-- |  o     |
-- | xox    |
-- |x       |
-- ----------


-- puzzle :: Result -> [Char] -> Result


-- -- Define the puzzle2 data structure
-- puzzle2 :: [String]
-- puzzle2 = ["AC DE",
--            "FBHIJ",
--            "KGLNO",
--            "PQMRS",
--            "UVWXT"]

-- -- Function to switch two characters at given coordinates
-- switchCharacters :: [String] -> (Int, Int) -> (Int, Int) -> [String]
-- switchCharacters grid (x1, y1) (x2, y2) =
--   if validCoordinates x1 y1 && validCoordinates x2 y2
--     then switchGridElements grid (x1, y1) (x2, y2)
--     else grid

-- -- Check if the given coordinates are valid for the grid
-- validCoordinates :: Int -> Int -> Bool
-- validCoordinates x y =
--   x >= 0 && x < length puzzle2 && y >= 0 && y < length (head puzzle2)

-- -- Switch two elements in the grid at the given coordinates
-- switchGridElements :: [String] -> (Int, Int) -> (Int, Int) -> [String]
-- switchGridElements grid (x1, y1) (x2, y2) =
--   if x1 == x2 then
--     let row1 = grid !! x1
--         elem1 = row1 !! y1
--         elem2 = row1 !! y2
--         newRow1 = replaceAtIndex y1 (replaceAtIndex y2 row1 elem1) elem2
--     in replaceAtIndex x1 grid newRow1
--   else
--     let row1 = grid !! x1
--         row2 = grid !! x2
--         elem1 = row1 !! y1
--         elem2 = row2 !! y2
--         newRow1 = replaceAtIndex y1 row1 elem2
--         newRow2 = replaceAtIndex y2 row2 elem1
--     in 
--       replaceAtIndex x1 (replaceAtIndex x2 grid newRow2) newRow1

-- -- Helper function to replace an element at a specific index in a list
-- replaceAtIndex :: Int -> [a] -> a -> [a]
-- replaceAtIndex index xs newElement =
--   take index xs ++ [newElement] ++ drop (index + 1) xs

-- -- Function to apply a function to a value (pipe operator)
-- (|>) :: a -> (a -> b) -> b
-- (|>) x f = f x

-- -- Define a type class for structures that can contain characters
-- class CharacterContainer a where
--   findCharCoordinates :: Char -> a -> [(Int, Int)]
--   switchCoordinates :: Char -> Char -> a -> a

-- -- Implement the CharacterContainer type class for a list of strings
-- instance CharacterContainer [String] where
--   findCharCoordinates char grid =
--     [(row, col) | (row, rowString) <- zip [0..] grid, (col, gridChar) <- zip [0..] rowString, gridChar == char]

-- -- Function to find first char in string
-- getFirstItem :: [a] -> a
-- getFirstItem (x:_) = x
-- getFirstItem [] = error "Empty list"

-- -- Function to substring except first char
-- substringExceptFirst :: String -> String
-- substringExceptFirst str = drop 1 str

-- -- Function to check if string is empty
-- isEmpty :: String -> Bool
-- isEmpty str = null str

-- -- returns first char of string
-- getFirstCharacter :: String -> Char
-- getFirstCharacter (x:_) = x
-- getFirstCharacter [] = error "Empty sequence"

-- -- apply switchCoordinates for each character in a sequence
-- applySwitchSequence :: String -> [String] -> [String]
-- applySwitchSequence sequence grid =
--   let 
--     newGrid = if isEmpty sequence then
--       grid  
--       else applySwitchSequence (substringExceptFirst sequence) (switchCharacters grid (getFirstItem (findCharCoordinates ' ' grid)) (getFirstItem(findCharCoordinates (getFirstCharacter sequence) grid)))
--     in 
--        newGrid

-- main :: IO ()
-- main = do
--   putStrLn "Origin Grid:"
--   mapM_ putStrLn puzzle2
--   -- let newGrid = switchCharacters puzzle2 (0, 0) (1, 1)
--   -- putStrLn "Modified Grid 1:"
--   -- mapM_ putStrLn newGrid
--   -- let newGrid2 = switchCharacters puzzle2 (0, 2) (1, 2)
--   -- putStrLn "Modified Grid 2:"
--   -- mapM_ putStrLn newGrid2
--   -- let newGrid3 = switchCharacters puzzle2 (0, 2) (0, 1)
--   -- putStrLn "Modified Grid 3:"
--   -- mapM_ putStrLn newGrid3

--   -- let inputSequence = "C"
--   -- -- inputSequence2 = "H"
--   -- let newGrid4 = applySwitchSequence inputSequence puzzle2
--   --     -- newGrid2 = applySwitchSequence inputSequence2 puzzle2
--   -- putStrLn "Modified Grid 4:"
--   -- mapM_ putStrLn newGrid4

--   let inputSequence2 = "CBGLMRST"
--   -- let inputSequence2 = "CBG"
--   let newGrid5 = applySwitchSequence inputSequence2 puzzle2
--       -- newGrid2 = applySwitchSequence inputSequence2 puzzle2
--   putStr "Modified Grid 5 - sequence: "
--   putStrLn inputSequence2
--   putStrLn ""
--   mapM_ putStrLn newGrid5

-- type Result = String

-- Function to create an empty game board
emptyBoard :: (Int, Int) -> [[Char]]
emptyBoard (rows, cols) = replicate rows (replicate cols ' ')

-- Function to update the game board with player moves
updateBoard :: [[Char]] -> [(Int, Int)] -> [[Char]]
updateBoard board [] = board
updateBoard board ((x, y):moves) =
  let player = if even (length moves) then 'x' else 'o'
      newRow = replaceAtIndex (y - 1) (board !! (x - 1)) player
      newBoard = replaceAtIndex (x - 1) board newRow
  in updateBoard newBoard moves

-- Function to convert the game board to a printable string
boardToString :: [[Char]] -> String
boardToString board =
  let border = replicate (length (head board) + 2) '-'
      rows = map (\row -> '|' : row ++ "|") board
  in unlines (border : rows ++ [border])

-- Function to replace an element at a specific index in a list
replaceAtIndex :: Int -> [a] -> a -> [a]
replaceAtIndex index xs newElement = take index xs ++ [newElement] ++ drop (index + 1) xs

-- Main function to simulate the Ticktack game
ticktack :: (Int, Int) -> [(Int, Int)] -> Result
ticktack (rows, cols) moves =
  let initialBoard = emptyBoard (rows, cols)
      finalBoard = updateBoard initialBoard moves
  in finalBoard

-- Function to pretty print the Ticktack game result
-- pp :: Result -> IO ()
-- pp = putStrLn

main :: IO ()
main = do
  let gameResult = ticktack (8, 8) [(1, 1), (8, 8), (2, 2), (3, 3), (4, 2), (3, 2)]
  pp gameResult
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}

-- above are recommended options for VSCode

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

-- Result:
--
-- ghci> main
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


-- result structure a print, not used
type Result = [String]
pp :: Result -> IO ()
pp x = putStr ( ((replicate (length x+2) '-' ))++ "\n" ++ concat (map ("|"++) (map (++"|\n") (reverse x))) ++ ((replicate (length x+2) '-' )) ++ "\n")

-- Function to create an empty game board
emptyBoard :: (Int, Int) -> [[Char]]
emptyBoard (rows, cols) = replicate rows (replicate cols ' ')

-- Function to update the game board with player moves
updateBoard :: [[Char]] -> [(Int, Int)] -> [[Char]]
updateBoard board [] = board
updateBoard board ((y, x):moves) =
  let player = if even (length moves) then 'o' else 'x'
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

main :: IO ()
main = do
  pp(ticktack (8,8) [(1,1),(8,8),(2,2),(3,3),(4,2),(3,2)])
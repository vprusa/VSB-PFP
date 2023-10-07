module Main
  where

main=putStrLn "Hello, World!"

type Result = [String]
pp :: Result -> IO ()
pp x = putStr (concat (map (++"\n") x))

-- 1 - Magic 15 Puzzle

-- Implement the function puzzle, it will simulate the game similar to 15 Puzzle. In our case, we have 25 squares, where 24 squares are ocupied by tiles with big case letters from 'A' to 'X'. One tile is free, it is denotated by ' '. In one move, you can move a tile (denotated by its letter) into the free one. The function gets the original configuration and a sequence of valid moves. It will output the resulting configuration. 

puzzle2 :: [String]
puzzle2 = ["AC DE",
           "FBHIJ",
           "KGLNO",
           "PQMRS",
           "UVWXT"]

-- puzzle :: Result -> [Char] -> Result

-- puzzle input steps = let
--   coordinates' = [ ( ri ) | (ch) <- steps] -- vezmu znak a vyprodukuji z nej jeho row a column hodnoty
--   in

-- findCharCoordinates ' '

-- findCharCoordinates :: Char -> [(Int, Int)]
-- findCharCoordinates char =
--   [(row, col) | (row, rowString) <- zip [0..] puzzle2, (col, gridChar) <- zip [0..] rowString, gridChar == char]

findCharCoordinates :: Char -> [(Int, Int)]
findCharCoordinates char =
  [(row, col) | (row, rowString) <- zip [0..] puzzle2, (col, gridChar) <- zip [0..] rowString, gridChar == char]
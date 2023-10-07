module Main
  where

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

-- puzzle input steps = let
--   coordinates' = [ ( ri ) | (ch) <- steps] -- vezmu znak a vyprodukuji z nej jeho row a column hodnoty
--   in

-- findCharCoordinates ' '

-- findCharCoordinates :: Char -> [(Int, Int)]
-- findCharCoordinates char =
--   [(row, col) | (row, rowString) <- zip [0..] puzzle2, (col, gridChar) <- zip [0..] rowString, gridChar == char]

-- findCharCoordinates :: Char -> [String] -> [(Int, Int)]
-- findCharCoordinates char grid =
--   [(row, col) | (row, rowString) <- zip [0..] puzzle2, (col, gridChar) <- zip [0..] rowString, gridChar == char]

-- findCharCoordinates :: Char -> [String] -> [(Int, Int)]
-- findCharCoordinates char grid =
--   [(row, col) | (row, rowString) <- zip [0..] grid, (col, gridChar) <- zip [0..] rowString, gridChar == char]

-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FunctionalDependencies #-}

-- -- Define a type class for structures that can contain characters
-- class CharacterContainer a where
--   findCharCoordinates :: Char -> a -> [(Int, Int)]
--   switchCoordinates :: (Int, Int) -> (Int, Int) -> a -> a

-- -- Implement the CharacterContainer type class for a list of strings
-- instance CharacterContainer [String] where
--   findCharCoordinates char grid =
--     [(row, col) | (row, rowString) <- zip [0..] grid, (col, gridChar) <- zip [0..] rowString, gridChar == char]
  
--   switchCoordinates (row1, col1) (row2, col2) grid =
--     let char1 = (grid !! row1) !! col1
--         char2 = (grid !! row2) !! col2
--         newRow1 = replaceAtIndex col1 (grid !! row1) char2
--         newRow2 = replaceAtIndex col2 (grid !! row2) char1
--         newGrid = replaceAtIndex row1 grid newRow1
--     in replaceAtIndex row2 newGrid newRow2

-- -- Helper function to replace an element at a specific index in a list
-- replaceAtIndex :: Int -> [a] -> a -> [a]
-- replaceAtIndex index xs newElement = take index xs ++ [newElement] ++ drop (index + 1) xs

-- -- Example puzzle data
-- puzzle2 :: [String]
-- puzzle2 = ["AC DE",
--            "FBHIJ",
--            "KGLNO",
--            "PQMRS",
--            "UVWXT"]

-- main :: IO ()
-- main = do
--   -- let charToFind = 'H'
--       -- coordinates = findCharCoordinates charToFind puzzle2
--   -- putStrLn ("Coordinates of '" ++ [charToFind] ++ "': " ++ show coordinates)

--   -- -- let newGrid = switchCoordinates (1, 2) (2, 2) puzzle2
--   -- -- let oldCoords = findCharCoordinates ' ' puzzle2
--   -- -- putStrLn (oldCoords)
--   -- let newGrid = switchCoordinates (1, 2)  (2, 2) puzzle2
--   -- putStrLn "Modified Grid:"
--   -- mapM_ putStrLn newGrid

--   let char1 = 'H'
--       char2 = 'L'
--       coordinates1 = findCharCoordinates char1 puzzle2
--       coordinates2 = findCharCoordinates char2 puzzle2
--   putStrLn ("Coordinates of '" ++ [char1] ++ "': " ++ show coordinates1)
--   putStrLn ("Coordinates of '" ++ [char2] ++ "': " ++ show coordinates2)

--   let newGrid = switchCoordinates char1 char2 puzzle2
--   putStrLn "Modified Grid:"
--   mapM_ putStrLn newGrid



-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FunctionalDependencies #-}

-- -- Define a type class for structures that can contain characters
-- class CharacterContainer a where
--   findCharCoordinates :: Char -> a -> [(Int, Int)]
--   switchCoordinates :: (Int, Int) -> (Int, Int) -> a -> a

-- -- Implement the CharacterContainer type class for a list of strings
-- instance CharacterContainer [String] where
--   findCharCoordinates char grid =
--     [(row, col) | (row, rowString) <- zip [0..] grid, (col, gridChar) <- zip [0..] rowString, gridChar == char]
  
--   switchCoordinates (row1, col1) (row2, col2) grid =
--     let char1 = (grid !! row1) !! col1
--         char2 = (grid !! row2) !! col2
--         newRow1 = replaceAtIndex col1 (grid !! row1) char2
--         newRow2 = replaceAtIndex col2 (grid !! row2) char1
--         newGrid = replaceAtIndex row1 grid newRow1
--     in replaceAtIndex row2 newGrid newRow2

-- -- Helper function to replace an element at a specific index in a list
-- replaceAtIndex :: Int -> [a] -> a -> [a]
-- replaceAtIndex index xs newElement = take index xs ++ [newElement] ++ drop (index + 1) xs

-- -- Example puzzle data
-- puzzle2 :: [String]
-- puzzle2 = ["AC DE",
--            "FBHIJ",
--            "KGLNO",
--            "PQMRS",
--            "UVWXT"]

-- main :: IO ()
-- main = do
--   -- let charToFind = 'H'
--       -- coordinates = findCharCoordinates charToFind puzzle2
--   -- putStrLn ("Coordinates of '" ++ [charToFind] ++ "': " ++ show coordinates)

--   -- -- let newGrid = switchCoordinates (1, 2) (2, 2) puzzle2
--   -- -- let oldCoords = findCharCoordinates ' ' puzzle2
--   -- -- putStrLn (oldCoords)
--   -- let newGrid = switchCoordinates (1, 2)  (2, 2) puzzle2
--   -- putStrLn "Modified Grid:"
--   -- mapM_ putStrLn newGrid

--   let char1 = 'H'
--       char2 = 'L'
--       coordinates1 = findCharCoordinates char1 puzzle2
--       coordinates2 = findCharCoordinates char2 puzzle2
--   putStrLn ("Coordinates of '" ++ [char1] ++ "': " ++ show coordinates1)
--   putStrLn ("Coordinates of '" ++ [char2] ++ "': " ++ show coordinates2)

--   -- let newGrid = switchCoordinates coordinates1[0] coordinates2[0] puzzle2
--   let newGrid = switchCoordinates coordinates1[0] coordinates2[0] puzzle2
--   putStrLn "Modified Grid:"
--   -- mapM_ putStrLn newGrid




{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- Define a type class for structures that can contain characters
class CharacterContainer a where
  findCharCoordinates :: Char -> a -> [(Int, Int)]
  switchCoordinates :: Char -> Char -> a -> a

-- Implement the CharacterContainer type class for a list of strings
instance CharacterContainer [String] where
  findCharCoordinates char grid =
    [(row, col) | (row, rowString) <- zip [0..] grid, (col, gridChar) <- zip [0..] rowString, gridChar == char]
  
  switchCoordinates char1 char2 grid =
    case (findCharCoordinates char1 grid, findCharCoordinates char2 grid) of
      ([coord1], [coord2]) ->
        let (row1, col1) = coord1
            (row2, col2) = coord2
            newRow1 = replaceAtIndex col1 (grid !! row1) char2
            newRow2 = replaceAtIndex col2 (grid !! row2) char1
            newGrid = replaceAtIndex row1 grid newRow1
        in replaceAtIndex row2 newGrid newRow2
      _ -> grid  -- If either char1 or char2 is not found or multiple occurrences are found, return the original grid

-- Helper function to replace an element at a specific index in a list
replaceAtIndex :: Int -> [a] -> a -> [a]
replaceAtIndex index xs newElement = take index xs ++ [newElement] ++ drop (index + 1) xs

-- Example puzzle data
puzzle2 :: [String]
puzzle2 = ["AC DE",
           "FBHIJ",
           "KGLNO",
           "PQMRS",
           "UVWXT"]

main :: IO ()
main = do
  let char1 = ' '
      char2 = 'H'
      coordinates1 = findCharCoordinates char1 puzzle2
      coordinates2 = findCharCoordinates char2 puzzle2
  putStrLn ("Coordinates of '" ++ [char1] ++ "': " ++ show coordinates1)
  putStrLn ("Coordinates of '" ++ [char2] ++ "': " ++ show coordinates2)

  let newGrid = switchCoordinates char1 char2 puzzle2
  putStrLn "Modified Grid:"
  mapM_ putStrLn newGrid

  


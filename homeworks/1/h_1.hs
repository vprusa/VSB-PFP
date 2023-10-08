{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
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






-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FunctionalDependencies #-}

-- -- Define a type class for structures that can contain characters
-- class CharacterContainer a where
--   findCharCoordinates :: Char -> a -> [(Int, Int)]
--   switchCoordinates :: Char -> Char -> a -> a

-- -- Implement the CharacterContainer type class for a list of strings
-- instance CharacterContainer [String] where
--   findCharCoordinates char grid =
--     [(row, col) | (row, rowString) <- zip [0..] grid, (col, gridChar) <- zip [0..] rowString, gridChar == char]

--   switchCoordinates char1 char2 grid
--     | char2 == ' ' =  -- Check if char2 is an empty space
--         case (findCharCoordinates char1 grid, findCharCoordinates char2 grid) of
--           ([coord1], [coord2]) ->
--             let (row1, col1) = coord1
--                 (row2, col2) = coord2
--                 newRow1 = replaceAtIndex col1 (grid !! row1) char2
--                 newRow2 = replaceAtIndex col2 (grid !! row2) char1
--                 newGrid = replaceAtIndex row1 grid newRow1
--             in replaceAtIndex row2 newGrid newRow2
--           _ -> grid  -- If either char1 or char2 is not found or multiple occurrences are found, return the original grid
--     | otherwise = grid  -- If char2 is not an empty space, do not perform the switch

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
-- -- main = do
-- --   let char1 = 'H'
-- --       char2 = ' '  -- Change char2 to an empty space
-- --       coordinates1 = findCharCoordinates char1 puzzle2
-- --       coordinates2 = findCharCoordinates char2 puzzle2
-- --   putStrLn ("Coordinates of '" ++ [char1] ++ "': " ++ show coordinates1)
-- --   putStrLn ("Coordinates of '" ++ [char2] ++ "': " ++ show coordinates2)

-- --   let newGrid = switchCoordinates char1 char2 puzzle2
-- --   putStrLn "Modified Grid:"
-- --   mapM_ putStrLn newGrid


--   let char1 = 'H'
--       char2 = ' '  -- Change char2 to an empty space
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
--   switchCoordinates :: Char -> Char -> a -> a

-- -- Implement the CharacterContainer type class for a list of strings
-- instance CharacterContainer [String] where
--   findCharCoordinates char grid =
--     [(row, col) | (row, rowString) <- zip [0..] grid, (col, gridChar) <- zip [0..] rowString, gridChar == char]

--   switchCoordinates char1 char2 grid
--     | char2 == ' ' =  -- Check if char2 is an empty space
--         case (findCharCoordinates char1 grid, findCharCoordinates char2 grid) of
--           ([coord1], [coord2]) ->
--             let (row1, col1) = coord1
--                 (row2, col2) = coord2
--                 newRow1 = replaceAtIndex col1 (grid !! row1) char2
--                 newRow2 = replaceAtIndex col2 (grid !! row2) char1
--                 newGrid = replaceAtIndex row1 grid newRow1
--             in replaceAtIndex row2 newGrid newRow2
--           _ -> grid  -- If either char1 or char2 is not found or multiple occurrences are found, return the original grid
--     | otherwise = grid  -- If char2 is not an empty space, do not perform the switch

-- -- Helper function to replace an element at a specific index in a list
-- replaceAtIndex :: Int -> [a] -> a -> [a]
-- replaceAtIndex index xs newElement = take index xs ++ [newElement] ++ drop (index + 1) xs

-- -- Function to apply switchCoordinates for each character and ' ' in a sequence
-- applySwitchSequence :: [Char] -> [String] -> [String]
-- applySwitchSequence sequence grid =
--   foldl (\grid' char -> switchCoordinates char ' ' grid') grid sequence

-- -- Example puzzle data
-- puzzle2 :: [String]
-- puzzle2 = ["AC DE",
--            "FBHIJ",
--            "KGLNO",
--            "PQMRS",
--            "UVWXT"]

-- main :: IO ()
-- main = do
--   -- let inputSequence = "H JKL"
--   -- let inputSequence = "CGLMRST"
--   putStrLn "Modified Grid:"
--   mapM_ putStrLn puzzle2
--   let inputSequence = "C"
--       newGrid = applySwitchSequence inputSequence puzzle2
--   putStrLn "Modified Grid:"
--   mapM_ putStrLn newGrid


-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FunctionalDependencies #-}

-- -- Define a type class for structures that can contain characters
-- class CharacterContainer a where
--   findCharCoordinates :: Char -> a -> [(Int, Int)]
--   switchCoordinates :: Char -> Char -> a -> a

-- -- Implement the CharacterContainer type class for a list of strings
-- instance CharacterContainer [String] where
--   findCharCoordinates char grid =
--     [(row, col) | (row, rowString) <- zip [0..] grid, (col, gridChar) <- zip [0..] rowString, gridChar == char]

--   switchCoordinates char1 char2 grid
--     | char2 == ' ' =  -- Check if char2 is an empty space
--         case (findCharCoordinates char1 grid, findCharCoordinates char2 grid) of
--           ([coord1], [coord2]) ->
--             let (row1, col1) = coord1
--                 (row2, col2) = coord2
--                 newRow1 = if row1 /= row2 
--                   then 
--                     replaceAtIndex col1 (grid !! row1) char2 
--                   else 
--                     replaceAtIndex col1 (grid !! row1) 'x'

--                 newRow2 = if row1 /= row2
--                   then 
--                     replaceAtIndex col2 (grid !! row2) char2 -- 'y' -- char1
--                   else do
--                     replaceAtIndex col1 (grid !! row1) char2 -- 'y' -- char1
--                     -- replaceAtIndex col1 (grid !! row2) char2 -- 'y' -- char1
--                 -- newRow1 = replaceAtIndex col1 (grid !! row1) char2
--                 -- newRow2 = replaceAtIndex col2 (grid !! row2) char1 -- 'y' -- char1
--                 -- newRow3 = replaceAtIndex col1 (grid !! newRow2) char1 -- char1 -- 'y' -- char1
--                 -- newRow2 = replaceAtIndex col (grid !! row1) char1 -- 'y' -- char1
--                 newGrid = replaceAtIndex row1 grid newRow1
--                 -- newGrid1 = replaceAtIndex row2 grid newRow2
--                 -- newGrid = replaceAtIndex row1 newGrid1 newRow1
--             -- in replaceAtIndex row2 newGrid newRow2
--             in replaceAtIndex row2 newGrid newRow2
--           -- ([coord1], [coord2]) ->
--           --   let (row1, col1) = coord1
--           --       (row2, col2) = coord2
--           --       newRow1 = replaceAtIndex col1 (grid !! row1) 'x'
--           --       newRow2 = replaceAtIndex col2 (grid !! row2) 'y'
--           --       newGrid = replaceAtIndex row1 grid newRow1
--           --   in replaceAtIndex row2 newGrid newRow2

--           _ -> grid  -- If either char1 or char2 is not found or multiple occurrences are found, return the original grid
--     | otherwise = grid  -- If char2 is not an empty space, do not perform the switch

-- -- Helper function to replace an element at a specific index in a list
-- replaceAtIndex :: Int -> [a] -> a -> [a]
-- replaceAtIndex index xs newElement = take index xs ++ [newElement] ++ drop (index + 1) xs

-- -- Function to apply switchCoordinates for each character in a sequence
-- applySwitchSequence :: String -> [String] -> [String]
-- applySwitchSequence sequence grid =
--   foldl (\grid' char -> if char /= ' ' then switchCoordinates char ' ' grid' else grid') grid sequence

-- -- Example puzzle data
-- puzzle2 :: [String]
-- puzzle2 = ["AC DE",
--            "FBHIJ",
--            "KGLNO",
--            "PQMRS",
--            "UVWXT"]

-- main :: IO ()
-- main = do
--   -- let inputSequence = "H JK L"
--   putStrLn "Modified Grid1:"  
--   mapM_ putStrLn puzzle2
--   -- let inputSequence = "H"
--   -- let inputSequence = "CGLMRST"
--   -- let inputSequence = "H"
--   let inputSequence = "C"
--       inputSequence2 = "H"
--       newGrid = applySwitchSequence inputSequence puzzle2
--       newGrid2 = applySwitchSequence inputSequence2 puzzle2
--   putStrLn "Modified Grid:"
--   mapM_ putStrLn newGrid

--   putStrLn "Modified Grid2:"
--   mapM_ putStrLn newGrid2


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
        -- row2 = grid !! x2
        elem1 = row1 !! y1
        elem2 = row1 !! y2
        -- newRow1 = replaceAtIndex y1 row1 elem2
        -- newRow2 = replaceAtIndex y2 row1 elem1
        newRow1 = replaceAtIndex y1 (replaceAtIndex y2 row1 elem1) elem2
        -- newRow3 = replaceAtIndex x2 grid -- newRow2
    -- in replaceAtIndex x1 grid newRow2
    in replaceAtIndex x1 grid newRow1
    -- in  replaceAtIndex x2 grid newRow2 
  else
    let row1 = grid !! x1
        row2 = grid !! x2
        elem1 = row1 !! y1
        elem2 = row2 !! y2
        newRow1 = replaceAtIndex y1 row1 elem2
        newRow2 = replaceAtIndex y2 row2 elem1
        -- newRow3 = replaceAtIndex x2 grid -- newRow2
    -- in replaceAtIndex x1 grid newRow2
    -- in replaceAtIndex x1 grid newRow2 
    in 
      replaceAtIndex x1 (replaceAtIndex x2 grid newRow2) newRow1
      -- replaceAtIndex x1 grid newRow1
      -- putStrLn newRow1
      --  replaceAtIndex x1 grid newRow1
    -- in  replaceAtIndex x2 grid newRow2 



-- Helper function to replace an element at a specific index in a list
replaceAtIndex :: Int -> [a] -> a -> [a]
replaceAtIndex index xs newElement =
  take index xs ++ [newElement] ++ drop (index + 1) xs

-- Function to apply a function to a value (pipe operator)
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

main :: IO ()
main = do
  putStrLn "Origin Grid:"
  mapM_ putStrLn puzzle2
  let newGrid = switchCharacters puzzle2 (0, 0) (1, 1)  -- Example: Switch (0,0) and (1,1)
  putStrLn "Modified Grid 1:"
  mapM_ putStrLn newGrid
  let newGrid2 = switchCharacters puzzle2 (0, 2) (1, 2)
  putStrLn "Modified Grid 2:"
  mapM_ putStrLn newGrid2
  let newGrid3 = switchCharacters puzzle2 (0, 2) (0, 1)
  putStrLn "Modified Grid 3:"
  mapM_ putStrLn newGrid3

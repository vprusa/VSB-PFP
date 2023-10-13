{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Main
  where
import Control.Monad.RWS (First(getFirst))
import Data.Binary.Get (isEmpty)

type Result = [String]
pp :: Result -> IO ()
pp x = putStr (concat (map (++"\n") x))


type Transition = (Int, Char, Int)
type Automaton = (Int, String, [Transition], Int, [Int])


ex1 :: Automaton 
ex1 = (3, "ab", [(0,'a',1), (0,'b',0), (1,'a',1), (1,'b',2), (2,'a',1), (2,'b',0)], 0, [2])

ex2 :: Automaton 
ex2 = (3, "ab", [(0,'a',1), (0,'a',0), (0,'b',0), (1,'b',2)], 0, [2])

-- Helper function to replace an element at a specific index in a list
replaceAtIndex :: Int -> [a] -> a -> [a]
replaceAtIndex index xs newElement =
  take index xs ++ [newElement] ++ drop (index + 1) xs

-- Function to apply a function to a value (pipe operator)
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

-- Function to print automata to standard output
printAutomaton :: Automaton -> IO ()
printAutomaton (states, alphabet, transitions, startState, acceptingStates) = do
    putStrLn $ "Number of states: " ++ show states
    putStrLn $ "Alphabet: " ++ alphabet
    putStrLn "Transitions: "
    mapM_ printTransition transitions
    putStrLn $ "Start state: " ++ show startState
    putStrLn $ "Accepting states: " ++ show acceptingStates
    where 
        printTransition (from, char, to) = 
            putStrLn $ "(" ++ show from ++ ", " ++ [char] ++ ", " ++ show to ++ ")"


-- Function to decide if input autmata is deterministic
isDeterministic:: Automaton  -> Bool
isDeterministic (states, alphabet, transitions, startState, acceptingStates) = let
        result = alphabet == ""
       in
        result


main :: IO ()
main = do
    putStrLn "Automaton 1: "
    printAutomaton ex1
    putStrLn "\nAutomaton 2: "
    printAutomaton ex2

    putStrLn ("Is #1 deterministic?: " ++ (if isDeterministic ex1 then "True" else "False"))


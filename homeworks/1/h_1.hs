{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Main
  where
import Control.Monad.RWS (First(getFirst))
import Data.Binary.Get (isEmpty)

-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-List.html#g:15 - partition - split by predicate
import Data.List (partition)
import Data.Type.Coercion (trans)

type Result = [String]
pp :: Result -> IO ()
pp x = putStr (concat (map (++"\n") x))


type Transition = (Int, Char, Int)
type Automaton = (Int, String, [Transition], Int, [Int])


ex1 :: Automaton
ex1 = (3, "ab", [(0,'a',1), (0,'b',0), (1,'a',1), (1,'b',2), (2,'a',1), (2,'b',0)], 0, [2])

ex2 :: Automaton
ex2 = (3, "ab", [(0,'a',1), (0,'a',0), (0,'b',0), (1,'b',2)], 0, [2])


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



-- Function that groups transitions by (from state, input symbol)
groupBySameFromAndChar :: [Transition] -> [[Transition]]
groupBySameFromAndChar [] = []
groupBySameFromAndChar (t : ts) =
  let (same, rest) = partition (isSameFromAndChar t) ts
   in (t : same) : groupBySameFromAndChar rest

-- Predicate function to check if two transitions have the same "from" state and input symbol
isSameFromAndChar :: Transition -> Transition -> Bool
isSameFromAndChar (from1, char1, _) (from2, char2, _) = from1 == from2 && char1 == char2


-- Function to decide if an automaton is deterministic
isDeterministic :: Automaton -> Bool
isDeterministic (states, alphabet, transitions, startState, acceptingStates) =
  -- all transitions have to be detemrinistic
  all isTransitionDeterministic (groupBySameFromAndChar transitions)
  where
    -- Check if transitions for a group are deterministic
    -- Helper function that takes list of transitions and returns Bool that determins if transition has for same start state single input character  
    isTransitionDeterministic :: [(Int, Char, Int)] -> Bool
    isTransitionDeterministic group = ((length group) == 1)


-- Second function runs automaton for given string and establishes, if given input string is accepted by the given automaton. It should work for both deterministic and non-deterministic finite automatons.

-- canContinue:: Automaton -> String -> Int -> Bool
-- canContinue (states, alphabet, transitions, startState, acceptingStates) (transIndex) (input) = 
--   let
--     trans = transitions[transIndex]
--     in 

-- Function that returns string except first character
substringExceptFirst :: String -> String
substringExceptFirst str = drop 1 str

-- Function that returns True if value is in aray
isValueInArray :: Eq a => a -> [a] -> Bool
isValueInArray value array = elem value array

-- nextTransition:: Transition -> Char -> Transition
-- nextTransition (from, char, to) char = 
--   let
--     in 

filterTransition:: [Transition] -> Int -> Char -> [Transition]
filterTransition transitions startState targetChar =
  filter (\(from, char, _) -> from == startState && char == targetChar) transitions

getNextState:: Transition -> Int
getNextState (from, char, to) = to

isAccepting:: Automaton -> String -> Bool
isAccepting (states, alphabet, transitions, startState, acceptingStates) (input) =
  let
    nextInput = drop 1 input
    curChar = input !! 0

    -- get next possible transitions as list
    currentStateTransitions = filterTransition transitions startState curChar

    canContinue =
      not (null input) &&
      not (null currentStateTransitions)
    isFinished = length nextInput == 0 && elem startState acceptingStates
    nextState = getNextState (head currentStateTransitions)
    result
      | isFinished = True
      | canContinue = isAccepting (states, alphabet, transitions, nextState, acceptingStates) nextInput
      | otherwise = False
 in
    result



main :: IO ()
main = do
  putStrLn "Automaton 1: "
  printAutomaton ex1
  putStrLn "\nAutomaton 2: "
  printAutomaton ex2

  putStrLn ("Is #1 deterministic?: " ++ (if isDeterministic ex1 then "True" else "False"))
  putStrLn ("Is #2 deterministic?: " ++ (if isDeterministic ex2 then "True" else "False"))

  putStrLn ("Is #1 accepting?: " ++ (if isAccepting ex1 "aab" then "True" else "False"))
  putStrLn ("Is #2 accepting?: " ++ (if isAccepting ex2 "aaa" then "True" else "False"))

  putStrLn ("Is #1 accepting?: " ++ (if isAccepting ex1 "ab" then "True" else "False"))
  putStrLn ("Is #1 accepting?: " ++ (if isAccepting ex1 "a" then "True" else "False"))
  putStrLn ("Is #1 accepting?: " ++ (if isAccepting ex1 "abb" then "True" else "False"))
  putStrLn ("Is #1 accepting?: " ++ (if isAccepting ex1 "b" then "True" else "False"))
  putStrLn ("Is #1 accepting?: " ++ (if isAccepting ex1 "abbbbb" then "True" else "False"))

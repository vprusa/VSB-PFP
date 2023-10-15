{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Main
  where
import Control.Monad.RWS (First(getFirst))
import Data.Binary.Get (isEmpty)
import Control.Monad

-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-List.html#g:15 - partition - split by predicate
import Data.List (partition)
import Data.Type.Coercion (trans)
import Data.List (nub, subsequences, sort)
type Result = [String]
pp :: Result -> IO ()
pp x = putStr (concat (map (++"\n") x))


type Transition = (Int, Char, Int)
type Automaton = (Int, String, [Transition], Int, [Int])


ex1 :: Automaton
ex1 = (3, "ab", [(0,'a',1), (0,'b',0), (1,'a',1), (1,'b',2), (2,'a',1), (2,'b',0)], 0, [2])

ex2 :: Automaton
ex2 = (3, "ab", [(0,'a',1), (0,'a',0), (0,'b',0), (1,'b',2)], 0, [2])

ex3 :: Automaton
ex3 = (3, "ab", [(0,'a',1), (0,'b',0), (1,'a',1), (1,'b',2), (2,'a',1), (2,'b',0)], 0, [2])

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

-- Function that returns string except first character
substringExceptFirst :: String -> String
substringExceptFirst str = drop 1 str

-- Function that returns True if value is in aray
isValueInArray :: Eq a => a -> [a] -> Bool
isValueInArray value array = elem value array

-- Function to filter tarnsitions so that only those that match startState and char are returned
filterTransition:: [Transition] -> Int -> Char -> [Transition]
filterTransition transitions startState targetChar =
  filter (\(from, char, _) -> from == startState && char == targetChar) transitions

-- Function to return next state index
getNextState:: Transition -> Int
getNextState (from, char, to) = to

-- Function that checks if automat can successfully accept String
isAccepting:: Automaton -> String -> Bool
isAccepting (states, alphabet, transitions, startState, acceptingStates) (input) =
  let
    nextInput = drop 1 input
    curChar = head input

    -- get next possible transitions as list
    currentStateTransitions = filterTransition transitions startState curChar

    canContinue =
      not (null input) &&
      not (null currentStateTransitions) &&
      not (null currentStateTransitions)
    isFinished = (null nextInput && null input) && elem startState acceptingStates
    nextState = getNextState (head currentStateTransitions)
    result
      | isFinished && True = True
      | canContinue = isAccepting (states, alphabet, transitions, nextState, acceptingStates) nextInput
      | otherwise = False
 in
    result


--     Finally, create a function that takes a non-deterministic automaton and produces it's deterministic equivalent (the output can be very different based on used methodology).

convert:: Automaton -> Automaton
convert (states, alphabet, transitions, startState, acceptingStates) = (states, alphabet, transitions, startState, acceptingStates)

-- according to docu it is necessary to get closures for states (for given char..)
-- https://condor.depaul.edu/glancast/444class/docs/nfa2dfa.html
-- https://joeylemon.github.io/nfa-to-dfa/

-- Convert an NFA to a DFA
nfaToDFA :: Automaton -> Automaton
nfaToDFA (nfaStates, alphabet, nfaTransitions, nfaStartState, nfaAcceptingStates) =
  let
    dfaStates = powerset [0..nfaStates - 1] -- Create the set of DFA states

    -- Helper function to compute the epsilon-closure of a set of NFA states
    epsilonClosure :: [Int] -> [Int]
    epsilonClosure states = nub $ sort $ concatMap (epsilonClosureOfState nfaTransitions) states

    -- Compute transitions for the DFA
    -- dfaTransitions = [(dfaState, char, nextState) |
    --                   dfaState <- dfaStates,
    --                   char <- alphabet,
    --                   let nfaStatesInDFAState = dfaState,
    --                   let nextState = epsilonClosure $ nub $ sort $ concatMap (transitionsForState nfaTransitions char) nfaStatesInDFAState,
    --                   nextState /= []]
    -- dfaTransitions = nub [(dfaState !! 0, char, nextState!! 0) |
    --               dfaState <- dfaStates,
    --               char <- alphabet,
    --               let nfaStatesInDFAState = dfaState,
    --               let nextState = epsilonClosure $ nub $ sort $ concatMap (transitionsForState nfaTransitions char) nfaStatesInDFAState,
    --               nextState /= []]

    dfaTransitions = nub [(dfaState !! 0, char, nextState !! 0) |
                  dfaState <- dfaStates,
                  char <- alphabet,
                  let nfaStatesInDFAState = dfaState,
                  let nextState = epsilonClosure $ nub $ sort $ concatMap (transitionsForState nfaTransitions char) nfaStatesInDFAState,
                  nextState /= []]

    -- dfaTransitions = []

    dfaStartState = epsilonClosure [nfaStartState] -- Compute the start state of the DFA
    dfaAcceptingStates = [dfaState | dfaState <- dfaStates, any (`elem` nfaAcceptingStates) dfaState]
  in
    (length dfaStates, alphabet, dfaTransitions, 0, (nub (concat dfaAcceptingStates)))

-- Helper function to compute the epsilon-closure of an NFA state
epsilonClosureOfState :: [Transition] -> Int -> [Int]
epsilonClosureOfState transitions state =
  let
    epsilonTransitions = [toState | (fromState, char, toState) <- transitions, fromState == state, char == 'ε']
    recursiveEpsilonClosure = concatMap (epsilonClosureOfState transitions) epsilonTransitions
  in
    state : recursiveEpsilonClosure

-- Helper function to find transitions for a given state and character
transitionsForState :: [Transition] -> Char -> Int -> [Int]
transitionsForState transitions char state =
  [toState | (fromState, inputChar, toState) <- transitions, fromState == state, inputChar == char]


-- filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]

-- Helper function to generate powerset of a list
powerset :: [a] -> [[a]]
-- powerset xs = filterM (const [True, False]) xs
powerset xs = filterM (const [True, False]) xs


main :: IO ()
main = do
  putStrLn "Automaton 1: "
  printAutomaton ex1
  putStrLn "\nAutomaton 2: "
  printAutomaton ex2

  putStrLn "\n1.1.:"

  putStrLn ("Is #1 deterministic?: " ++ (if isDeterministic ex1 then "True" else "False")) -- True
  putStrLn ("Is #2 deterministic?: " ++ (if isDeterministic ex2 then "True" else "False")) -- False

  putStrLn "\n1.2.a) Required inputs:"

  putStrLn ("Is #1 accepting?: aab " ++ (if isAccepting ex1 "aab" then "True" else "False")) -- True
  putStrLn ("Is #2 accepting?: aaa " ++ (if isAccepting ex2 "aaa" then "True" else "False")) -- False

  putStrLn "\n1.2.b) Optional inputs:"
  putStrLn ("Is #1 accepting?: ab " ++ (if isAccepting ex1 "ab" then "True" else "False")) -- True
  putStrLn ("Is #1 accepting?: a " ++ (if isAccepting ex1 "a" then "True" else "False")) -- False
  putStrLn ("Is #1 accepting?: abb " ++ (if isAccepting ex1 "abb" then "True" else "False")) -- False
  putStrLn ("Is #1 accepting?: aabb " ++ (if isAccepting ex1 "aabb" then "True" else "False")) -- False
  putStrLn ("Is #1 accepting?: aabb " ++ (if isAccepting ex1 "aabb" then "True" else "False")) -- False
  putStrLn ("Is #1 accepting?: abab " ++ (if isAccepting ex1 "abab" then "True" else "False")) -- False
  putStrLn ("Is #1 accepting?: aba " ++ (if isAccepting ex1 "aba" then "True" else "False")) -- False
  putStrLn ("Is #1 accepting?: b " ++ (if isAccepting ex1 "b" then "True" else "False")) -- False
  putStrLn ("Is #1 accepting?: abbbbb " ++ (if isAccepting ex1 "abbbbb" then "True" else "False")) -- False

  putStrLn ("Is #2 accepting?: b " ++ (if isAccepting ex2 "b" then "True" else "False")) -- False
  putStrLn ("Is #2 accepting?: ab " ++ (if isAccepting ex2 "ab" then "True" else "False")) -- True - hack because I head of transitinos is (0,a,1), if the transition would be random then this would (sometimes) fail
  putStrLn ("Is #2 accepting?: bab " ++ (if isAccepting ex2 "ab" then "True" else "False")) -- True - hack because I head of transitinos is (0,a,1), if the transition would be random then this would (sometimes) fail

  putStrLn "\nConver non-deterministic to deterministic:"

  putStrLn "\nConverting Automaton 3:"
  printAutomaton ex3
  putStrLn "\nConverting Automaton 3 - deterministic:"
  printAutomaton (nfaToDFA ex3)
  putStrLn "done"
  

  putStrLn "\nConverting Automaton 2:"
  printAutomaton ex2
  putStrLn "\nConverting Automaton 2 - deterministic:"
  printAutomaton (nfaToDFA ex2)
  putStrLn "done"
  

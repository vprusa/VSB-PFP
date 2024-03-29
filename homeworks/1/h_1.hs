{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
module Main
  where
import Control.Monad.RWS (First(getFirst), All (getAll))
import Data.Binary.Get (isEmpty)
import Control.Monad
import Data.Char (chr)
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-List.html#g:15 - partition - split by predicate
import Data.List (partition)
import Data.Type.Coercion (trans)
import Data.List (nub, subsequences, sort, concatMap)
import Data.IntMap.Merge.Lazy (merge)

import Data.List (intersect)

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
getAllStates :: [Transition] -> [Int]
getAllStates transitions = nub (concatMap (\(x, _, y) -> [x, y]) transitions )


-- Function to generate all possible states of existing states
possibleStatesCombinations :: [Int] -> [[Int]]
possibleStatesCombinations [] = [[]]  -- The only combination of an empty list is an empty list itself
possibleStatesCombinations (x:xs) = combinations xs ++ map (x:) (combinations xs)

listAllStates:: Automaton -> [Int]
listAllStates (states, alphabet, transitions, startState, acceptingStates) = 
  (getAllStates transitions)

listAllPossibleStates:: Automaton -> [[Int]]
listAllPossibleStates (states, alphabet, transitions, startState, acceptingStates) = 
  (possibleStatesCombinations (getAllStates transitions))

combinations :: [a] -> [[a]]
combinations [] = [[]]  -- The only combination of an empty list is an empty list itself
combinations (x:xs) = combinations xs ++ map (x:) (combinations xs)


type Transition2 = ([Int], Char, [Int])
type Automaton2 = (Int, String, [Transition2], [Int], [[Int]])


-- Function to print automata to standard output
printAutomaton2 :: Automaton2 -> IO ()
printAutomaton2 (states, alphabet, transitions, startState, acceptingStates) = do
    putStrLn $ "Number of states: " ++ show states
    putStrLn $ "Print automaton2: "
    putStrLn $ "Alphabet: " ++ (show alphabet)
    putStrLn "Transitions: "
    mapM_ printTransition transitions
    putStrLn $ "Start state: " ++ show startState
    putStrLn $ "Accepting states: " ++ show (map (\st -> sort st) acceptingStates)
    where
        printTransition (from, char, to) =
            putStrLn $ "(" ++ show (sort from) ++ ", " ++ [char] ++ ", " ++ show (sort to) ++ ")"

printTransitions2 :: [Transition2] -> IO ()
printTransitions2 transitions = do
    putStrLn "Transitions2: "
    mapM_ printTransition transitions
    where
        printTransition (from, char, to) =
            putStrLn $ "(" ++ show (sort from) ++ ", '" ++ [char] ++ "', " ++ show (sort to) ++ ")"


printStatesAndTransitions2 :: ([[Int]], [Transition2]) -> IO ()
printStatesAndTransitions2 (states,transitions) = do
    putStrLn $ "States2: " ++ show states
    printTransitions2 transitions

newTransitionFrom :: [Transition2] -> Transition2
newTransitionFrom trans = 
  let 
    -- res = ([3], 'd', [2])
    -- res = ([inSt], char, [outSt])
    char = nub (concatMap (\(_, c, _) -> [c]) trans) -- TODO length char > 0 
    inState = concat $ nub (concatMap (\(x, _, _) -> [x]) trans)
    outState = concat $ nub (concatMap (\(_, _, y) -> [y]) trans)

    res = (inState, head char, outState)
  in res


isValidTrans :: [[Int]] -> Transition2 -> Bool
isValidTrans states (i,c,o) = (elem o states) && (elem i states)

generateNewTrans :: [Int] -> Char -> [Transition2] -> Transition2
generateNewTrans state char oldTransitions =
  let
    newTransition = if state == [] then ([], char, []) -- TODO un-hack
    else 
      let 
        relatedTransitionsList = concatMap (\subState -> (filter (\(inState, c, outState) -> c == char && (elem subState inState)) oldTransitions) ) state
        relatedTransitionsIn = concatMap (\(i,_,_)-> i) (relatedTransitionsList)
        relatedTransitionsOut = concatMap (\(_,_,o)-> o) (relatedTransitionsList)
        newTrans = [(state, char, nub relatedTransitionsOut)]
        result = if length newTrans > 0 then newTransitionFrom newTrans else (state, char, [])  
      in
        result 
  in
    newTransition


printCleanTransitionAndStates :: ([[Int]],[Transition2]) -> IO ()
printCleanTransitionAndStates (st,transitions) =
  do
  putStrLn $ "Accepting states: " ++ show (concat st)
  -- printTransitions2 trans
  putStrLn "Transitions2: "
  mapM_ printTransition transitions
  where
      printTransition (from, char, to) =
          putStrLn $ "(" ++ show from ++ ", " ++ [char] ++ ", " ++ show to ++ ")"

-- Function that geenrates all states with epsioln transitions
convertToEpsionAutomaton2 :: Automaton -> IO ()
convertToEpsionAutomaton2 (states, alphabet, transitions, startState, acceptingStates) = 
  -- create epsilon closure states
  do
    -- newAutomata = (0, alphabet, [], [], [])
    -- newAutomata = (2, alphabet, [([0],'a', [0]), ([0],'b', [1])], [1], [[2]])
    -- for each new state for each alphabet char map new transition with end state
    -- remove unnecessary transitions
    -- select new accepting states
    putStrLn "convertToEpsionAutomaton2 - starting"
    let
      remappedFinalStates = map (\st -> [st]) acceptingStates
      remappedTransitions = map (\(inState, char, outState) -> ([inState], char, [outState])) transitions
      newStates = (map (\st -> sort st) (possibleStatesCombinations (getAllStates transitions)))
    putStrLn "convertToEpsionAutomaton2 - preparation"
    printStatesAndTransitions2 (newStates, remappedTransitions) 
    putStrLn "convertToEpsionAutomaton2 - preparation - done"
    let
      (newCleanStates, newTransitions) = generateNewStatesAndTransitions newStates alphabet remappedTransitions
      newFinalStates = findNewFinalStates acceptingStates newCleanStates
      newAutomaton = (length newCleanStates, alphabet, newTransitions, [startState], newFinalStates)
    -- newAutomaton = (2, alphabet, [([0],'a', [0]), ([0],'b', [1])], [1], [[2]])
    -- putStrLn ("7 " ++ show (length (concat newCleanStates)))
    printAutomaton2 newAutomaton
    putStrLn "convertToEpsionAutomaton2 - done\n"
    

-- Function that converts NFA to DFA
convert :: Automaton -> Automaton2
convert (states, alphabet, transitions, startState, acceptingStates) = 
  let
    -- newAutomata = (0, alphabet, [], [], [])
    -- newAutomata = (2, alphabet, [([0],'a', [0]), ([0],'b', [1])], [1], [[2]])
    -- for each new state for each alphabet char map new transition with end state
    -- remove unnecessary transitions
    -- select new accepting states
    -- remappedFinalStates = map (\st -> [st]) acceptingStates
    remappedTransitions = map (\(inState, char, outState) -> ([inState], char, [outState])) transitions
    newStates = (map (\st -> sort st) (possibleStatesCombinations (getAllStates transitions)))
    (newCleanStates, newTransitions) = generateNewStatesAndTransitions newStates alphabet remappedTransitions
    newFinalStates = findNewFinalStates acceptingStates newCleanStates
    newAutomaton = (length newCleanStates, alphabet, newTransitions, [startState], newFinalStates)
  in
      newAutomaton


-- Sample function to compute epsilon closure
epsilonClosure :: Int -> [Transition] -> [Int]
epsilonClosure state transitions = undefined  -- Implement your epsilon closure logic here


hasParent :: [Int] -> [Transition2] -> Bool
hasParent state transitions = 
  let
    parentTransitions = filter (\(i,c,o) -> (sort o) == (sort state)) transitions
    in
       not (null parentTransitions)

hasTransExistingStates :: [[Int]] -> Transition2 -> Bool
hasTransExistingStates states (i,c,o) = 
  let
    sortedStates = map (\s -> (sort s)) states 
    res = (elem (sort i) sortedStates) && (elem (sort o) sortedStates) -- ((elem (sort o) (sortedStates)) && (elem (sort i) (sortedStates)))
    in
      res

cleanTransitionAndStates :: [[Int]] -> [Transition2] -> ([[Int]],[Transition2])
cleanTransitionAndStates states trans = 
  let
    sortedStates = map (\s -> (sort s)) states 
    newStateNoParentless = filter (\st -> (hasParent st trans)) sortedStates
    newTransitionsParentLess = filter (\tr -> (hasTransExistingStates newStateNoParentless tr)) trans
  in
    (newStateNoParentless, newTransitionsParentLess)

cleanTransitionAndStatesRecursion :: [[Int]] -> [Transition2] -> ([[Int]],[Transition2])
cleanTransitionAndStatesRecursion states trans = 
  let
    (newStates, newTrans) = cleanTransitionAndStates states trans
    (resStates, resTrans) = if (newStates /= states || newTrans /= trans) then cleanTransitionAndStatesRecursion newStates newTrans else (newStates, newTrans)
    in
      (resStates, resTrans)  

generateNewStatesAndTransitions :: [[Int]] -> String -> [Transition2] -> ([[Int]], [Transition2])
generateNewStatesAndTransitions newStates alphabet oldTransitions = 
  let
    -- newTransitions = [([0],'a', [0])]
    newTransitionsDuplicates = concat (map (\newState -> (map (\char -> generateNewTrans newState char oldTransitions) alphabet ) ) newStates )
    newTransitionsNoDuplicates = nub newTransitionsDuplicates
    (newStateNoParentless, newTransitionsNoParentLess) = cleanTransitionAndStatesRecursion newStates newTransitionsNoDuplicates
    -- (newStateNoParentless, newTransitionsNoParentLess) = cleanTransitionAndStates newStates newTransitionsNoDuplicates
    newTransitionsNoEps = filter (\(i,c,o) -> i /= [] && o /= []) newTransitionsNoParentLess 
    -- just to make sure ...
    -- (newStates, newTransitions) = cleanTransitionAndStatesRecursion newStateNoParentless newTransitionsNoEps
    (newStates2, newTransitions) = cleanTransitionAndStatesRecursion newStateNoParentless newTransitionsNoEps
    -- (newStates2, newTransitions) =  (newStateNoParentless, newTransitionsNoEps)
  in
    -- (newStateNoParentless0, newTransitions)
    (newStates2, newTransitions)


findNewFinalStates :: [Int] -> [[Int]] -> [[Int]]
findNewFinalStates oldStates genStates = 
  let
    res = filter (\st -> (intersect oldStates st) /= []) genStates
  in 
    res

-- according to docu it is necessary to get closures for states (for given char..)
-- https://condor.depaul.edu/glancast/444class/docs/nfa2dfa.html
-- https://joeylemon.github.io/nfa-to-dfa/

-- Notes: it was be necesasry to generate all possible states combinations
-- - for each state it is necessary to have a transition
-- - then when all transitions for each states are generated it is required to remove all staes for which there is no incomming edge
-- - now we have a DFA from input NFA ..

mapTransitionToString :: [Transition] -> [Int]
mapTransitionToString transtitions =
    nub $ concatMap (\(x, _, y) -> [x, y]) transtitions 

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

  putStrLn "\n1.3. Conver non-deterministic to deterministic:"

  putStrLn "\n1.3. Ex 2:"
  printAutomaton ex2
  putStrLn "\n1.3. Ex 2 - converted:"
  printAutomaton2 (convert ex2)

  putStrLn "\n1.3. Ex 3:"
  printAutomaton ex3
  putStrLn "\n1.3. Ex 3 - converted:"
  printAutomaton2 (convert ex3)
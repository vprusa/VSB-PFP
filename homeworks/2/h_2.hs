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


-- Regular expressions are usually defined recursively as follows:
--     Elementary regular expressions are: just symbols or empty string ϵ {\displaystyle \epsilon } (or empty set but we do not really care about this option).
--     We can build new regular expressions from existing one using three basic operations: concatenation, alternation and iteration.
-- We will accomodate this definition into following data structure: 

data RegExpr = Epsilon
             | Symbol Char
             | Iteration RegExpr
             | Concat RegExpr RegExpr
             | Alter RegExpr RegExpr deriving (Eq, Show)

-- ex1 :: Automaton
-- ex1 = (3, "ab", [(0,'a',1), (0,'b',0), (1,'a',1), (1,'b',2), (2,'a',1), (2,'b',0)], 0, [2])

-- ex2 :: Automaton
-- ex2 = (3, "ab", [(0,'a',1), (0,'a',0), (0,'b',0), (1,'b',2)], 0, [2])

-- ex3 :: Automaton
-- ex3 = (3, "ab", [(0,'a',1), (0,'b',0), (1,'a',1), (1,'b',2), (2,'a',1), (2,'b',0)], 0, [2])

ex1 :: Automaton 
ex1 = (3, "ab", [(0,'a',1), (0,'b',0), (1,'a',1), (1,'b',2), (2,'a',1), (2,'b',0)], 0, [2])

ex2 :: Automaton 
ex2 = (3, "ab", [(0,'a',1), (0,'a',0), (0,'b',0), (1,'b',2)], 0, [2])


-- As an example we can use following regular expression: 
reg1 :: RegExpr 
reg1 = Concat (Concat (Iteration (Alter (Symbol 'a') (Symbol 'b'))) (Symbol 'a')) (Symbol 'b')

-- Your task will be to create a function convert:
--    Function convert takes a regular expression and produces an equivalent finite automaton.


-- Function to convert regex to string
regex2Str :: RegExpr -> String
regex2Str Epsilon = "ε"
regex2Str (Symbol c) = "Symbol '" ++ [c] ++ "'"
regex2Str (Iteration re) = "Iteration (" ++ regex2Str re ++ ")"
regex2Str (Concat re1 re2) = "Concat (" ++ regex2Str re1 ++ ") (" ++ regex2Str re2 ++ ")"
regex2Str (Alter re1 re2) = "Alter (" ++ regex2Str re1 ++ ") (" ++ regex2Str re2 ++ ")"

regex2Str2 :: RegExpr -> String
regex2Str2 Epsilon = "ε"
-- regex2Str2 (Symbol c) = "" ++ [c] ++ ""
-- regex2Str2 (Iteration re) = "" ++ regex2Str2 re ++ ""
-- regex2Str2 (Concat re1 re2) = "" ++ regex2Str2 re1 ++ "" ++ regex2Str2 re2 ++ ""
-- regex2Str2 (Alter re1 re2) = "" ++ regex2Str2 re1 ++ "" ++ regex2Str2 re2 ++ ""
regex2Str2 (Symbol c) = "" ++ [c] ++ ""
-- regex2Str2 (Iteration (Symbol c)) = "" ++ [c] ++ "*"
-- regex2Str2 (Concat re1 re2) = "(" ++ regex2Str2 re1 ++ ")(" ++ regex2Str2 re2 ++ ")"
-- regex2Str2 (Alter re1 re2) = "(" ++ regex2Str2 re1 ++ ")|(" ++ regex2Str2 re2 ++ ")"
regex2Str2 (Iteration re) = "(" ++ regex2Str2 re ++ ")*"
regex2Str2 (Concat re1 re2) = "" ++ regex2Str2 re1 ++ "" ++ regex2Str2 re2 ++ ""
regex2Str2 (Alter re1 re2) = "" ++ regex2Str2 re1 ++ "|" ++ regex2Str2 re2 ++ ""

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


-- Function that returns string except first character
substringExceptFirst :: String -> String
substringExceptFirst str = drop 1 str

-- Function that returns True if value is in aray
isValueInArray :: Eq a => a -> [a] -> Bool
isValueInArray value array = elem value array

mapTransitionToString :: [Transition] -> [Int]
mapTransitionToString transtitions =
    nub $ concatMap (\(x, _, y) -> [x, y]) transtitions 


convert :: RegExpr -> Automaton

-- Function to convert regular expression to (deterministic?) finite state automata
-- https://www.geeksforgeeks.org/conversion-of-regular-expression-to-finite-automata/
convert reg = 
  let
    test = (3, "ab", [(0,'a',1), (0,'b',0), (1,'a',1), (1,'b',2), (2,'a',1), (2,'b',0)], 0, [2])
    in
      test

-- Function to find the maximum state number in a list of transitions
findMaxStateNumber :: [Transition] -> Int
findMaxStateNumber transitions = maximum $ concatMap (\(q1, _, q2) -> [q1, q2]) transitions

-- Function to convert transitions in one list so that its states are 
-- disjunct to the other list states 
convertToDistinctTrans :: [Transition] -> [Transition] -> [Transition]
convertToDistinctTrans orig toConv =
  let
    -- list number of states in orig transitions and 
    maxInOrig = findMaxStateNumber orig 
    resConv = map (\(tci, tcc, tco) -> (tci + maxInOrig + 1, tcc, tco + maxInOrig + 1 )) toConv
    in
      -- toConv
      resConv

-- Function to convert a RegExpr into an Automaton
-- https://www.javatpoint.com/automata-conversion-of-re-to-fa
convertToAutomaton :: RegExpr -> Automaton
convertToAutomaton Epsilon = (1, "ε", [(0, 'ε', 0)], 0, [0])
convertToAutomaton (Symbol c) = (2, [c], [(0, c, 1)], 0, [1])
convertToAutomaton (Concat re1 re2) =
    let (q01, s01, ts01, qf01, afs01) = convertToAutomaton re1
        (q02, s02, ts02, qf02, afs02) = convertToAutomaton re2
        aboveMaxOrig = (findMaxStateNumber ts01) + 1
        q02_fix = aboveMaxOrig
        s02_fix = s02
        ts02_fix = convertToDistinctTrans ts01 ts02
        qf02_fix = qf02 + aboveMaxOrig
        afs02_fix = map (\afs02i -> afs02i + aboveMaxOrig) afs02
        -- newTransitions = [(qf01, 'ε', q02)]
        newTransitions = map (\fs01 -> (fs01, 'ε', q02_fix)) afs01 -- [(qf01, 'ε', q02)]
        newAcceptingStates = afs02_fix
    in
        (q01, s01 ++ s02_fix, ts01 ++ ts02_fix ++ newTransitions, qf02_fix, newAcceptingStates)
convertToAutomaton (Alter re1 re2) =
        (-1, "x", [(100,'x',200)], 100, [200])
convertToAutomaton (Iteration re) =
       (-1, "y", [(400,'y',500)], 400, [500])

-- Note: Intermediate steps may require a finite automaton with epsilon steps. 
-- Resulting automaton may differ based on used algorithms. 

-- ghci>convert reg1
-- (3, "ab", [(0,'a',1), (0,'a',0), (0,'b',0), (1,'b',2)], 0, [2])

main :: IO ()
main = do
  -- putStrLn "Automaton 1: "
  -- printAutomaton ex1
  putStrLn "\n2.1. Conver regex to automata:"
  putStrLn "\nregex"
  putStrLn (regex2Str reg1)
  putStrLn "\nregex - plain"
  putStrLn (regex2Str2 reg1)
  putStrLn "\nautomata:"
  printAutomaton (convert reg1)
  putStrLn "\nautomata2:"
  printAutomaton (convertToAutomaton reg1)
  putStrLn "\ntest 1:"
  -- printAutomaton (convertToAutomaton (Concat (Concat (Iteration (Alter (Symbol 'a') (Symbol 'b'))) (Symbol 'a')) (Symbol 'b')))
  printAutomaton (convertToAutomaton (Symbol 'a'))
  printAutomaton (convertToAutomaton (Concat (Symbol 'a') (Symbol 'a')))
  printAutomaton (convertToAutomaton (Concat (Symbol 'a') (Symbol 'b')))
  printAutomaton (convertToAutomaton (Concat (Symbol 'a') ( Concat (Symbol 'b') (Symbol 'a'))))

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# LANGUAGE OverloadedStrings #-}

-- module Main
  -- where
-- import Control.Monad.RWS (First(getFirst), All (getAll))
import Data.Binary.Get (isEmpty)
import Control.Monad ()
import Data.Char (chr)
-- https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-List.html#g:15 - partition - split by predicate
import Data.List (partition)
import Data.Type.Coercion (trans)
import Data.List (nub, subsequences, sort, concatMap)
import Data.IntMap.Merge.Lazy (merge)

import Web.Scotty

-- import qualified Data.Set as Set
-- import Data.List (nub, sort)

-- import Data.List (intersect)


-- main :: IO ()
-- main = do
-- putStrLn "Web app"

-- main = scotty 3000 $
--   get "/:word" $ do
--     beam <- param "word"
--     -- beam <- "word"
--     -- html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
--     html $ mconcat ["<h1>Scotty, ", " me up!</h1>"]

-- {-# LANGUAGE OverloadedStrings #-}
-- import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
    get "/:name" $ do
        -- name <- param "name"
        name <- queryParam "word"
        html $ mconcat ["<h1>Hello, ", name, "!</h1>"]

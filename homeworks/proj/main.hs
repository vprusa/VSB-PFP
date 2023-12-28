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
import Data.List (intercalate)
import qualified Data.Text.Lazy as TL

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

-- Define the User data type
data User = User {
    userId :: Int,
    userNick :: String,
    userEmail :: String
} deriving (Show, Eq)

-- Define the Item data type
data Item = Item {
    itemId :: Int,
    itemUserId :: Int,
    itemName :: String
} deriving (Show, Eq)

createUsers :: Int -> Int -> [User]
createUsers n offset = [User i (nickPattern i) (emailPattern i) | i <- [offset..(offset + n)]]
    where
        nickPattern i = "user-" ++ show i
        emailPattern i = intercalate "@" ["user-" ++ show i, "example.com"]

createItems :: Int -> Int -> Int -> [Item]
createItems userId n offset = [Item i userId (namePattern i) | i <- [offset..(offset + n)]]
    where
        namePattern i = "item_name-" ++ show (offset + i)

createTestUsers :: [User]
createTestUsers = createUsers 5 0

-- createItemsForUser :: User -> Int -> Int -> [Item]
-- createItemsForUser (User userId nick mail) offset max = 
-- [Item i userId ("Item" ++ show i) | i <- [1..5]]  -- Example, creates 5 items per user
-- createItems userId offset max

-- create recursively items (count = max - offset) given user id
createItemsForUser :: Int -> Int -> Int -> [Item]
createItemsForUser user offset max = createItemsHelper user offset max 1
    where
        createItemsHelper :: Int -> Int -> Int -> Int -> [Item]
        createItemsHelper _ _ 0 _ = []
        createItemsHelper userId offset max count =
            Item (offset + count) userId ("Item" ++ show (offset + count))
            : createItemsHelper userId offset (max - 1) (count + 1)

-- generates users items
generateUsersItems :: [User] -> [[Item]]
generateUsersItems users = map (\(User userId nick name) -> createItemsForUser userId 0 10) users


-- createTestItems :: [User] -> [[Item]]
-- createTestItems users =  map (\(userId, _, _) -> createItems userId 1 0 ) users
-- createTestItems users =  map (\u -> createItems u.userId 1 0 ) users

main :: IO ()
main = scotty 3000 $ do
    -- get "/:name" $ do
    get "/" $ do
        -- name <- param "name"
        -- name <- queryParam "name"
        html $ mconcat ["<h1>Home Page for Users and their Carts!</h1>", 
          "<br><a href='/users'> list all users</a>",
          "<br><a href='/carts'> list all carts</a>"
          ]
    get "/users" $ do
      html $ mconcat (map (\u -> TL.pack (show u ++ "<br>") ) createTestUsers)




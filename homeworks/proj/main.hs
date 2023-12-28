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
import Data.Typeable
import Data.Text.Lazy (Text)

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
createTestUsersItems :: [User] -> [[Item]]
createTestUsersItems users = map (\(User userId nick name) -> createItemsForUser userId 0 2) users


-- createTestItems :: [User] -> [[Item]]
-- createTestItems users =  map (\(userId, _, _) -> createItems userId 1 0 ) users
-- createTestItems users =  map (\u -> createItems u.userId 1 0 ) users


-- findUserByName :: String -> [User] -> Maybe User
-- findUserByName name users = head $ filter (\(User id nick mail) -> nick == name ) users 

-- with userNick find Just user or Nothing 
findUserByName :: String -> [User] -> Maybe User
findUserByName name users = 
    case filter (\u -> userNick u == name) users of
        [] -> Nothing
        (x:_) -> Just x

-- converts foud user to string
findUserByNameString :: String -> [User] -> String
findUserByNameString name users = show (findUserByName name users)

-- filter items by user id
findUserItemsByUserId :: Int -> [Item] -> [Item]
findUserItemsByUserId userId items = filter (\(Item itemId itemUserId itemName) -> itemUserId == userId) items
  -- (findUserByName name users)

-- getUserId :: Maybe User -> Int
-- getUserId user = case user of
--         Just (User) -> User
--         Nothing -> -1
--         -- userId user

getUserId :: Maybe User -> Int
getUserId maybeUser = case maybeUser of
    Just user -> userId user
    Nothing   -> -1

itemsToStr :: [Item] -> String
itemsToStr items = show $ map (\i -> TL.pack $ show i) items 

-- getUserIdSafe :: Maybe User -> String
-- -- getUserIdSafe user = userId user
-- getUserIdSafe maybeUser = case maybeUser of
--     Just user -> Just (userId user)
--     Nothing   -> "" -- Nothing

-- getUserIdSafe :: Maybe User -> String
-- -- getUserIdSafe user = userId user
-- getUserIdSafe maybeUser = case maybeUser of
--     Just user -> Just (userId user)
--     Nothing   -> "" -- Nothing


main :: IO ()
main = scotty 3000 $ do
    let 
      testUsers = createTestUsers
      testItems = concat $ createTestUsersItems testUsers -- TODO persistency?
    -- get "/:name" $ do
    get "/" $ do
        -- name <- param "name"
        -- name <- queryParam "name"
        html $ mconcat [
          "<h1>Home Page for Users and their Carts!</h1>", 
          "<br><a href='/users'> list all users</a>",
          "<br><a href='/items'> list all items</a>"
          ]

    get "/users" $ do
      html $ mconcat (map (\u -> TL.pack (show u ++ "<br>") ) testUsers)

    get "/items" $ do
      html $ mconcat (map (\i -> TL.pack (show i ++ "<br>") ) testItems)

    -- http://localhost:3000/user/?name=test
    get "/user/:name" $ do
      name <- queryParam "name"
      let userStr = TL.pack $ findUserByNameString name testUsers
      -- let userId = getUserId $ head $ getUserId $ findUserByName name testUsers
      let userId = getUserId $ findUserByName name testUsers 
      -- let userId = getUserId $ head $ getUserId user --  $ findUserByName name testUsers
      -- let userItemsStr = getUserIdSafe user
      let userItemsStr = TL.pack $ itemsToStr $ findUserItemsByUserId userId testItems -- testItems
      -- let userItemsStr = case user of
        -- Just user -> "" -- Just (userId user)
        -- Nothing   -> ""
        -- if not user then "" else "" -- TL.pack $ findUserItemsByUserId user is
      html $ mconcat ["<h1>User Info</h1>", 
        "<br>Searched Name: ", TL.pack name,
        -- "<br>Found: ", (TL.pack (show (findUserByName "user-1" testUsers) ++ "<br>"))
        -- "<br>Found: ", (TL.pack (show (findUserByName (show name) testUsers) ++ "<br>"))
        -- "<br>Found: ", (TL.pack (show (findUserStringByName (show name) testUsers) ++ "<br>"))
        -- "<br>Found: ", (TL.pack (show (findUserStringByName (show name) testUsers) ++ "<br>"))
        -- "<br>Found: ", (TL.pack (show (findUserStringByName (show name) testUsers) ++ "<br>"))
        -- "<br>Found: ", (TL.pack (show (findUserStringByName ( show $ TL.pack $ (show name) ) testUsers ) ++ "<br>"))
        -- "<br>Found: ", (TL.pack (show (findUserStringByName ( show name ) testUsers ) ++ "<br>"))
        "<br>Found: ", userStr,  "<br>",
        "<br>Cart items: ", userItemsStr,  "<br>"
        -- "<br>Found: ", ( TL.pack $ show $ typeOf name)
        -- "<br>Found: ", ( TL.pack $ show $ typeOf name)
        -- "<br>Found: ", (TL.pack (show (findUserByName name testUsers) ++ "<br>"))] --mconcat (map (\i -> TL.pack (show i ++ "<br>") ) testItems)
        ]
    


hello :: Text -> Text
hello s = "hello " <> s

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant if" #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Database.PostgreSQL.Simple

-- Notes:
-- https://webbureaucrat.gitlab.io/articles/writing-a-hello-world-web-app-in-haskell-with-scotty/
-- https://hackage.haskell.org/package/scotty-0.21/docs/Web-Scotty.html

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

-- create N ussers with id offsetted by offset 
createUsers :: Int -> Int -> [User]
createUsers n offset = [User i (nickPattern i) (emailPattern i) | i <- [offset..(offset + n)]]
    where
        nickPattern i = "user-" ++ show i
        emailPattern i = intercalate "@" ["user-" ++ show i, "example.com"]

-- create n items with userId and id ofsetted by offset 
createItems :: Int -> Int -> Int -> [Item]
createItems userId n offset = [Item i userId (namePattern i) | i <- [offset..(offset + n)]]
    where
        namePattern i = "item_name-" ++ show (offset + i)

createTestUsers :: [User]
createTestUsers = createUsers 5 0

-- create recursively items (count = max - offset) given user id
-- here it could be considered to deal with State in functional programming using recursion (seems like a nasty solution)
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

getUserId :: Maybe User -> Int
getUserId maybeUser = case maybeUser of
    Just user -> userId user
    Nothing   -> -1

itemsToStr :: [Item] -> String
itemsToStr items = show $ map (\i -> TL.pack $ show i) items 

-- update a users email based on their nick
-- updateUserEmail :: Int -> String -> [User] -> [User]
-- updateUserEmail userId newEmail users = map updateEmail users
--   where
--     updateEmail /user@(User id nick' email)
--       | id == userId = User id nick' newEmail
--       | otherwise = user

-- update a users email based on their id
updateUserEmail :: Int -> String -> [User] -> [User]
updateUserEmail userId newEmail users = map updateEmail users
  where
    updateEmail user@(User id nick email)
      | id == userId = User id nick newEmail
      | otherwise = user

main :: IO ()
main = do
    let 
      testUsers = createTestUsers
      testItems = concat $ createTestUsersItems testUsers

      -- conn <- connect defaultConnectInfo {
      conn = connect defaultConnectInfo {
                connectHost = "localhost",   -- or your database host
                connectDatabase = "vsb",
                connectUser = "vsb",
                connectPassword = "vsb"
            }
    
    -- Your database operations go here

    -- close conn  -- Close the connection when done


    scotty 3000 $ do
      get "/" $ do
          html $ mconcat [
            "<h1>Home Page for Users and their Carts!</h1>", 
            "<br><a href='/users'> list all users</a>",
            "<br><a href='/items'> list all items</a>"
            ]

      get "/users" $ do
        html $ mconcat (map (\u -> TL.pack (show u ++ "<br>") ) testUsers)

      get "/items" $ do
        html $ mconcat (map (\i -> TL.pack (show i ++ "<br>") ) testItems)

      -- http://localhost:3000/user/?name=user-1
      get "/user/:name" $ do
        -- name <- param "name"
        name <- queryParam "name"
        let userStr = TL.pack $ findUserByNameString name testUsers
        let userId = getUserId $ findUserByName name testUsers 
        let userItemsStr = TL.pack $ itemsToStr $ findUserItemsByUserId userId testItems -- testItems
        html $ mconcat ["<h1>User Info</h1>", 
          "<br>Searched Name: ", TL.pack name,
          "<br>Found: ", userStr,  "<br>",
          "<br>Cart items: ", userItemsStr,  "<br>"
          ]

      get "/setUser/:name" $ do
        name <- queryParam "name"
        let userStr = TL.pack $ findUserByNameString name testUsers
        let userId = getUserId $ findUserByName name testUsers 
        let userItemsStr = TL.pack $ itemsToStr $ findUserItemsByUserId userId testItems -- testItems
        html $ mconcat ["<h1>User Info</h1>", 
          "<br>Searched Name: ", TL.pack name,
          "<br>Found: ", userStr,  "<br>",
          "<br>Cart items: ", userItemsStr,  "<br>"
          ]
 
      -- http://localhost:3000/updateUserEmail/?userName=user-1&newEmail=testmail
      -- Endpoint to update user email
      get "/updateUserEmail/:userName:newName" $ do
        userName <- queryParam "userName"
        newEmail <- queryParam "newEmail"
        -- users <- updateUsers -- liftIO $ readIORef usersRef
        let 
          -- newEmail = "nomail" -- queryParam "newEmail"
          user = findUserByName userName testUsers
          userId = getUserId user -- (findUserByName userName testUsers)
        let 
          updatedUsers = updateUserEmail userId newEmail testUsers 
          newUser = findUserByName userName updatedUsers
        -- let testUsers = findUserByName userName testUsers
        -- liftIO $ writeIORef usersRef updatedUsers
        html $ TL.pack $ "Updated email for old user: <br>" ++ (show user) 
          ++ "<br> to new user:<br>" ++ (show newUser)
      
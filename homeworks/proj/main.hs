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

import Control.Monad.IO.Class (liftIO)

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
-- import Database.PostgreSQL.Simple (field)

import Data.Int (Int64)

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

-- getUsers :: Connection -> IO [User]
-- getUsers conn = query_ conn "SELECT user_id, user_nick, user_email FROM users"

instance FromRow User where
    fromRow = User <$> field <*> field <*> field

getAllUsers :: Connection -> IO [User]
getAllUsers conn = query_ conn "SELECT user_id, user_nick, user_email FROM users"

deleteUser :: Connection -> Int -> IO Int64
deleteUser conn userId = 
  execute conn "DELETE FROM users WHERE user_id = ?" (Only userId)

addUser :: Connection -> Item -> IO Int64
addUser conn item = execute conn 
    "INSERT INTO users (user_nick, user_email) VALUES (?, ?)" 
    (itemUserId item, itemName item)

instance FromRow Item where
    fromRow = Item <$> field <*> field <*> field

getAllItems :: Connection -> IO [Item]
getAllItems conn = query_ conn "SELECT item_id, item_user_id, item_name FROM items"


getAllItemsForUser :: Connection -> Int -> IO [Item]
getAllItemsForUser conn userId = 
  query conn "SELECT item_id, item_user_id, item_name FROM items WHERE item_user_id = ?" (Only userId)


deleteItem :: Connection -> String -> Int -> IO Int64
deleteItem conn itemName userId = 
  execute conn "DELETE FROM items WHERE item_name = ? AND item_user_id = ?" (itemName, userId)

addItem :: Connection -> Item -> IO Int64
addItem conn item = execute conn 
    "INSERT INTO items (item_user_id, item_name) VALUES (?, ?)"  (itemUserId item, itemName item)

-- update a users email based on their id
updateUserEmail :: Int -> String -> [User] -> [User]
updateUserEmail userId newEmail users = map updateEmail users
  where
    updateEmail user@(User id nick email)
      | id == userId = User id nick newEmail
      | otherwise = user

main :: IO ()
main = do
    conn <- connect (defaultConnectInfo {
    -- connInfo <- (defaultConnectInfo {
      -- connInfo = defaultConnectInfo {
                -- localhost needs change in /var/lib/pgsql/data/pg_hba.conf
                connectHost = "",  -- or your database host
                connectDatabase = "vsb",
                connectUser = "vsb",
                connectPassword = "vsb"
            })
    -- testUsers <- getAllUsers conn
    -- testItems <- getAllItems conn

    scotty 3000 $ do
      get "/" $ do
          html $ mconcat [
            "<h1>Home Page for Users and their Carts!</h1>", 
            "<br><a href='/users'> list all users</a>",
            "<br><a href='/items'> list all items</a>"
            ]
      -- http://localhost:3000/users
      get "/users" $ do
        -- conn <- connect connInfo
        allUsers <- liftIO $ getAllUsers conn 
        html $ mconcat (map (\u -> TL.pack (show u ++ "<br>") ) allUsers)

      -- http://localhost:3000/items
      get "/items" $ do
        allItems <- liftIO $ getAllItems conn 
        html $ mconcat (map (\i -> TL.pack (show i ++ "<br>") ) allItems)

      -- http://localhost:3000/user/?name=user-db-1
      get "/user/:name" $ do
        -- name <- param "name"
        name <- queryParam "name"
        allUsers <- liftIO $ getAllUsers conn 
        allItems <- liftIO $ getAllItems conn 

        let userStr = TL.pack $ findUserByNameString name allUsers
        let userId = getUserId $ findUserByName name allUsers
        let userItemsStr = TL.pack $ itemsToStr $ findUserItemsByUserId userId allItems
        let res = close conn
        html $ mconcat ["<h1>User Info</h1>", 
          "<br>Searched Name: ", TL.pack name,
          "<br>Found: ", userStr,  "<br>",
          "<br>Cart items: ", userItemsStr,  "<br>"
          ]

      -- user
      get "/addUser/:name:mail" $ do
        name <- queryParam "name"
        allUsers <- liftIO $ getAllUsers conn 
        allItems <- liftIO $ getAllItems conn 

        let userStr = TL.pack $ findUserByNameString name allUsers
        let userId = getUserId $ findUserByName name allUsers 
        let userItemsStr = TL.pack $ itemsToStr $ findUserItemsByUserId userId allItems
        let res = close conn
        html $ mconcat ["<h1>AddedUser Info</h1>", 
          "<br>Searched Name: ", TL.pack name,
          "<br>Found: ", userStr,  "<br>",
          "<br>Cart items: ", userItemsStr,  "<br>"
          ]
          
      -- http://localhost:3000/deleteUser/?name=user-db-1
      get "/deleteUser/:name" $ do
        -- name <- param "name"
        name <- queryParam "name"
        allUsers <- liftIO $ getAllUsers conn 

        let userStr = TL.pack $ findUserByNameString name allUsers
        let userId = getUserId $ findUserByName name allUsers
        allItems <- liftIO $ getAllItemsForUser conn userId
        let resDelItems = map ( \item itemId itemUserId itemName -> deleteItem itemName itemUserId) allItems
        let resDelUser = deleteUser conn userId
        let res = close conn
        html $ mconcat ["<h1>User Info</h1>", 
          "<br>Searched Name: ", TL.pack name,
          "<br>Deleted: ", userStr,  "<br>"
          ]

      -- adds new item to user's cart
      get "/addItemToCart/:userName:itemName" $ do
        userName <- queryParam "userName"
        itemName <- queryParam "itemName"
        allUsers <- liftIO $ getAllUsers conn 
        allItems <- liftIO $ getAllItems conn 

        -- hammer solution
        let userStr = TL.pack $ findUserByNameString userName allUsers
        let userId = getUserId $ findUserByName userName allUsers

        let oldUserItemsStr = TL.pack $ itemsToStr $ findUserItemsByUserId userId allItems -- testItems
        let newItem = Item 0 userId itemName
        itemAddedRes <- liftIO $ addItem conn newItem
        allNewItems <- liftIO $ getAllItems conn 
        let res = close conn
        let newUserItemsStr = TL.pack $ itemsToStr $ findUserItemsByUserId userId allNewItems -- testItems

        html $ mconcat ["<h1>User Info</h1>", 
          "<br>Searched Name: ", TL.pack userName,
          "<br>Found: ", userStr,  "<br>",
          "<br>Old Cart items: ", oldUserItemsStr,  "<br>",
          "<br>New Cart items: ", newUserItemsStr,  "<br>"
          ]


      -- adds new item to user's cart
      get "/deleteItemFromCart/:userName:itemName" $ do
        userName <- queryParam "userName"
        itemName <- queryParam "itemName"
        allUsers <- liftIO $ getAllUsers conn 

        -- hammer solution
        let userStr = TL.pack $ findUserByNameString userName allUsers
        let userId = getUserId $ findUserByName userName allUsers

        -- get user items by user id        
        userItems <- liftIO $ getAllItemsForUser conn userId
        let res = deleteItem conn itemName userId

        let oldUserItemsStr = TL.pack $ itemsToStr $ findUserItemsByUserId userId userItems
        let oldItem = Item 0 userId itemName
        
        allNewItems <- liftIO $ getAllItemsForUser conn userId 

        let res = close conn

        let newUserItemsStr = TL.pack $ itemsToStr $ findUserItemsByUserId userId allNewItems

        html $ mconcat ["<h1>User Info</h1>", 
          "<br>Searched Name: ", TL.pack userName,
          "<br>Found: ", userStr,  "<br>",
          "<br>Old Cart items: ", oldUserItemsStr,  "<br>",
          "<br>New Cart items: ", newUserItemsStr,  "<br>"
          ]          
          
      -- http://localhost:3000/updateUserEmail/?userName=user-1&newEmail=testmail
      -- Endpoint to update user email
      get "/updateUserEmail/:userName:newName" $ do
        userName <- queryParam "userName"
        newEmail <- queryParam "newEmail"

        allUsers <- liftIO $ getAllUsers conn 
        
        let 
          user = findUserByName userName allUsers
          userId = getUserId user
        let 
          updatedUsers = updateUserEmail userId newEmail allUsers
          newUser = findUserByName userName updatedUsers
        let res = close conn
        html $ TL.pack $ "Updated email for old user: <br>" ++ (show user) 
          ++ "<br> to new user:<br>" ++ (show newUser)
      
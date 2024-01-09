haskell web app using scotty web framework


On fedora Problem with

```
stack install scotty
```

```
Warning: Stack has not been tested with GHC versions above 9.6, and using 9.6.3, this may
         fail.

Warning: Stack has not been tested with Cabal versions above 3.10, but version 3.10.1.0 was
         found, this may fail.
[3 of 3] Linking /home/jdoe/.stack/setup-exe-cache/x86_64-linux/tmp-Cabal-simple_6HauvNHV_3.10.1.0_ghc-9.6.3
/usr/bin/ld.gold: error: cannot find -lgmp
collect2: error: ld returned 1 exit status
ghc-9.6.3: `gcc' failed in phase `Linker'. (Exit code: 1)

Error: [S-6374]
       While building simple Setup.hs (scroll up to its section to see the error) using:
       ~/.ghcup/ghc/9.6.3/bin/ghc -rtsopts -threaded -clear-package-db -global-package-db -hide-all-packages -package base -main-is StackSetupShim.mainOverride -package Cabal-3.10.1.0 /home/jdoe/.stack/setup-exe-src/setup-6HauvNHV.hs /home/jdoe/.stack/setup-exe-src/setup-shim-6HauvNHV.hs -o /home/jdoe/.stack/setup-exe-cache/x86_64-linux/tmp-Cabal-simple_6HauvNHV_3.10.1.0_ghc-9.6.3
       Process exited with code: ExitFailure 1 
```

resolved with:

```
dnf install gmp-devel -y

```

or just
```
cabal install scotty
```


### Prepare DB

- install psql,
- create user and DB named `vsb`, set role, set auth with password.

to `/var/lib/pgsql/data/pg_hba.conf` (or simillar depending on Arch) add:

```
local   all             all                                     md5
host    all             all             127.0.0.1/32            md5
```
or smth like this...
then use

```
    conn <- connect defaultConnectInfo {
                -- localhost needs change in /var/lib/pgsql/data/pg_hba.conf
                connectHost = "",  -- or your database host
                connectDatabase = "vsb",
                connectUser = "vsb",
                connectPassword = "vsb"
            }
```

```
dnf install postgresql-devel -y
# for libpq-devel-*

cabal install postgresql-simple
```


```
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
```

```
CREATE TABLE users (
    user_id SERIAL PRIMARY KEY,
    user_nick VARCHAR(255) NOT NULL,
    user_email VARCHAR(255) NOT NULL
);

CREATE TABLE items (
    item_id SERIAL PRIMARY KEY,
    item_user_id INTEGER NOT NULL,
    item_name VARCHAR(255) NOT NULL,
    FOREIGN KEY (item_user_id) REFERENCES users(user_id)
);
```


```

insert into users values (1, 'user-db-1', 'user-db-1@example.org');
insert into users values (2, 'user-db-2', 'user-db-2@example.org');
insert into users values (3, 'user-db-3', 'user-db-3@example.org');



insert into items values (1, 2, 'user-db-2-item-1');
insert into items values (2, 3, 'user-db-3-item-2');
insert into items values (3, 3, 'user-db-3-item-3');
insert into items values (4, 3, 'user-db-3-item-4');

```

Sample curl:


list users
`http://localhost:3000/users`

```
User {userId = 0, userNick = "user-db-1", userEmail = "user@example.org"}
User {userId = 1, userNick = "user-db-2", userEmail = "user-db-2@example.org"}
User {userId = 3, userNick = "user-db-3", userEmail = "user-db-3@example.org"}
```

list items
`http://localhost:3000/items`

```
Item {itemId = 1, itemUserId = 0, itemName = "Item1"}
Item {itemId = 2, itemUserId = 0, itemName = "Item2"}
Item {itemId = 1, itemUserId = 1, itemName = "Item1"}
Item {itemId = 2, itemUserId = 1, itemName = "Item2"}
Item {itemId = 1, itemUserId = 3, itemName = "Item1"}
Item {itemId = 2, itemUserId = 3, itemName = "Item2"}
```

show user detail with items
`http://localhost:3000/user/?name=user-db-2`

```
User Info

Searched Name: user-db-2
Found: Just (User {userId = 1, userNick = "user-db-2", userEmail = "user-db-2@example.org"})

Cart items: ["Item {itemId = 1, itemUserId = 1, itemName = \"user-db-2-item-1\"}"]
```


add user
`http://localhost:3000/addUser/?name=user-db-test&email=user-db-test@test.test`
```
```

delete user
`http://localhost:3000/deleteUser/?name=user-db-1`
```
```

add item to user
`http://localhost:3000/addItemToCart/?userName=user-db-3&itemName=test-user-db-1-item-1`
`http://localhost:3000/addItemToCart/?userName=user-db-3&itemName=test-user-db-1-item-3`

```
TODO
```

delete user
`http://localhost:3000/deleteUser/?name=user-db-1`

remove item for user
`http://localhost:3000/deleteItemFromCart/?name=user-db-3&itemName=test-user-db-1-item-3`


#

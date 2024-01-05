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
- create user and DB named `vsb`

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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Queries where

import Database.SQLite.Simple.Types
import Text.RawString.QQ

insertUser :: Query
insertUser =
  "INSERT INTO users\
  \ VALUES (?, ?, ?, ?, ?, ?)"

updateUser :: Query
updateUser = [r|
UPDATE users
SET username = :username,
    shell = :shell,
    homeDirectory = :homeDirectory,
    realName = :realName,
    phone = :phone
WHERE username = :whereUsername
|]

createUsersTable :: Query
createUsersTable = [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT, homeDirectory TEXT,
   realName TEXT, phone TEXT)
|]

selectAllUsers :: Query
selectAllUsers =
  "SELECT * from users"

selectUser :: Query
selectUser =
  "SELECT * from users where username = ?"

deleteUser :: Query
deleteUser =
  "DELETE from users where username = ?"

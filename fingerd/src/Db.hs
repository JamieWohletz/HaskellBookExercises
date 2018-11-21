{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module Db where

import Control.Exception
import Data.Text (Text)
import Data.Typeable
import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types

import qualified Queries as Q

data User =
  User {
      userId :: Integer
    , username :: Text
    , shell :: Text
    , homeDirectory :: Text
    , realName :: Text
    , phone :: Text
  } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) =
    toRow (id_, username, shell, homeDir, realName, phone)

data DuplicateData =
  DuplicateData
  deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow =
  (Null, Text, Text, Text, Text, Text)

mkUser :: Text -> Text -> Text -> Text -> Text -> User
mkUser = User 0

createDatabase :: IO ()
createDatabase = do
  conn <- open "finger.db"
  execute_ conn Q.createUsersTable
  execute conn Q.insertUser meRow
  rows <- query_ conn Q.selectAllUsers
  mapM_ print (rows :: [User])
  SQLite.close conn
  where
    meRow :: UserRow
    meRow =
      (Null, "callen", "/bin/zsh", "/home/callen", "Chris Allen", "555-123-4567")

getAllUsers :: (FromRow row) => Connection -> IO [row]
getAllUsers conn = query_ conn Q.selectAllUsers

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
  results <- query conn Q.selectUser (Only username)
  case results of
    [] -> return $ Nothing
    [user] -> return $ Just user
    _ -> throwIO DuplicateData

userToRow :: User -> UserRow
userToRow User{..} =
  (
    Null,
    username,
    shell,
    homeDirectory,
    realName,
    phone
  )

addUser :: Connection -> User -> IO ()
addUser conn usr = do
  execute conn Q.insertUser $ userToRow usr

putUser :: Connection -> -- db connection
              Text    -> -- username
              User    -> -- new user data
              Bool    -> -- whether to create new user if not exists
              IO Bool
putUser conn uname (user@User{..}) createNew = do
  maybeUser <- getUser conn uname
  case maybeUser of
    (Just u) -> executeNamed conn Q.updateUser params >> return True
    Nothing ->
      if createNew
      then addUser conn user >> return True
      else return False
  where
    params =
      [
        ":username" := username,
        ":shell" := shell,
        ":homeDirectory" := homeDirectory,
        ":realName" := realName,
        ":phone" := phone,
        ":whereUsername" := uname
      ]

delUser :: Connection -> Text -> IO ()
delUser conn username = do
  execute conn Q.deleteUser (Only username)
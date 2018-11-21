{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.Socket hiding (close, recv)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Network.Socket.ByteString (recv, sendAll)
import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Text.Trifecta (Result(..))

import Db (User(..))
import qualified Db
import qualified Parse

recvLine :: Socket -> IO ByteString
recvLine = go BS.empty
  where
    go s soc = do
      msg <- recv soc 1024
      if ((not $ BS.null msg) && BS.last msg == '\n') then
        return (s `BS.append` msg)
      else
        case msg of
          "\b" -> if BS.null s then go s soc else go (BS.init s) soc
          _ -> go (s `BS.append` msg) soc

returnUsers :: Connection -> Socket -> IO ()
returnUsers dbConn soc = do
  rows <- Db.getAllUsers dbConn
  let usernames        = map username rows
      newlineSeparated =
        T.concat $
        intersperse "\n" usernames
  sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) = BS.concat
  ["Login: ", e username, "\t\t\t\t",
   "Name: ", e realName, "\n",
   "Directory: ", e homeDir, "\t\t\t",
   "Shell: ", e shell, "\n"]
  where e = encodeUtf8

returnUser :: Connection -> Socket -> Text -> IO ()
returnUser dbConn soc username = do
  maybeUser <- Db.getUser dbConn (T.strip username)
  case maybeUser of
    Nothing -> do
      putStrLn
        ("Couldn't find matching user for username: "
        ++ (show username))
      return ()
    Just user ->
      sendAll soc (formatUser user)

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- recv soc 1024
  case msg of
    "\r\n" -> returnUsers dbConn soc
    name ->
      returnUser dbConn soc
      (decodeUtf8 name)

handleDbQuery :: Connection -> Socket -> IO ()
handleDbQuery dbConn soc = do
  msg <- recvLine soc
  putStrLn $ "DB: Received query: " ++ show msg
  case Parse.parseDbQuery msg of
    Success q -> 
      case q of
        Parse.Put user -> do
          Db.putUser dbConn (username user) user True
          sendAll soc "Successfully put user."
        Parse.Del uname -> do
          Db.delUser dbConn uname
          sendAll soc "Successfully deleted user (if they existed)."
    Failure _ -> sendAll soc "Invalid query."

handleQueries :: Connection -> Socket -> IO ()
handleQueries dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling query"
  handleQuery dbConn soc
  sClose soc

handleDbQueries :: Connection -> Socket -> IO ()
handleDbQueries dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "DB: Handling connection..."
  handleDbQuery dbConn soc
  sClose soc

fingerServer :: IO ()
fingerServer = withSocketsDo $ do
  addrinfos <-
    getAddrInfo
    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
    Nothing (Just "79")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 1
  conn <- open "finger.db"
  putStrLn "Finger server started on port 79."
  handleQueries conn sock
  SQLite.close conn
  sClose sock

dbServer :: IO ()
dbServer = withSocketsDo $ do
  addrinfos <-
    getAddrInfo
    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
    Nothing
    (Just "7777")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 1
  conn <- open "finger.db"
  putStrLn "DB server started on port 7777."
  handleDbQueries conn sock
  SQLite.close conn
  sClose sock

main :: IO ()
main = do
  _ <- forkIO fingerServer
  _ <- forkIO dbServer
  threadDelay 100
  putStrLn "--------------------\
            \Press enter to exit.\
            \--------------------"
  _ <- getLine
  return ()
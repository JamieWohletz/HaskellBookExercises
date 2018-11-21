{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Environment (getArgs, getProgName)
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)

main :: IO ()
main = withSocketsDo $ do
  addr <- resolve "127.0.0.1" "7777"
  E.bracket (open addr) close dbClient
  where
    resolve host port = do
      let hints = defaultHints { addrSocketType = Stream }
      addr:_ <- getAddrInfo (Just hints) (Just host) (Just port)
      return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      return sock

sendDelQuery :: Socket -> [String] -> IO ()
sendDelQuery sock [uname] = do
  sendAll sock $ C.pack $ "del " ++ uname ++ "\r\n"
sendDelQuery _ _ = putStrLn "Invalid number of arguments for `del` operation. Expected 1 arg."

sendPutQuery :: Socket -> [String] -> IO ()
sendPutQuery sock (uname:shell:home:name:phone:[]) =
  sendAll sock $ C.pack $ "put "
    ++ uname ++ ";"
    ++ shell ++ ";"
    ++ home ++ ";"
    ++ name ++ ";"
    ++ phone ++ "\r\n"
sendPutQuery _ _ = putStrLn "Invalid number of arguments for `put` operation. Expected 5 args."

echoResponse :: Socket -> IO ()
echoResponse sock = do
  msg <- recv sock 1024
  C.putStrLn msg

dbClient :: Socket -> IO ()
dbClient sock = do
  args <- getArgs
  progName <- getProgName
  case args of
    ("-d":xs) -> sendDelQuery sock xs >> echoResponse sock
    ("-p":xs) -> sendPutQuery sock xs >> echoResponse sock
    _ -> putStrLn $ "Usage: " ++ progName ++ " [-d username] [-p username shell home name phone]"
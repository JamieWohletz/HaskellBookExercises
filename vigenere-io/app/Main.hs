module Main where

import System.Environment (getArgs, getProgName)
import System.IO (
    stdin,
    stdout,
    stderr,
    hClose,
    hPutStr,
    hGetChar,
    hIsEOF,
    hWaitForInput
  )
import System.Exit (exitFailure)

import Lib

invalidModeMessage :: String
invalidModeMessage = "Please provide a valid mode: -e for encrypt, -d for decrypt."

usageMessage :: String -> String
usageMessage progName = "Usage: " ++ progName ++ " [-e | -d] key"

readStdIn :: IO String
readStdIn = do
  inputReceived <- hWaitForInput stdin 5000
  if inputReceived then
    go ""
  else
    hPutStr stderr "No input string received after 5 seconds." >>
    exitFailure
  where
    go :: String -> IO String
    go str = do
      eof <- hIsEOF stdin
      if eof then
        return str
      else
        do
          ch <- hGetChar stdin
          go (str ++ [ch])

closeHandles = hClose stdin >> hClose stdout >> hClose stderr

runProgram :: [String] -> IO ()
runProgram (x:key:xs)
  | x == "-e" = do
    toEncrypt <- readStdIn
    hPutStr stdout $ vigenere toEncrypt key
    closeHandles
  | x == "-d" = do
    toDecrypt <- readStdIn
    hPutStr stdout $ unVigenere toDecrypt key
    closeHandles
  | otherwise = putStrLn invalidModeMessage

main :: IO ()
main = do
  args <- getArgs
  case length args == 2 of
    False -> do
      progName <- getProgName
      putStrLn $ usageMessage progName
    True -> runProgram $ args
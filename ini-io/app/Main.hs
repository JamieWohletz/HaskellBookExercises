module Main where

import qualified Data.Map.Strict as M
import System.Directory (listDirectory, setCurrentDirectory)
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

usageMessage :: String -> String
usageMessage programName = "Usage: " ++ programName ++ " directory"

printUsageMessage :: IO ()
printUsageMessage = do
  progName <- getProgName
  putStrLn $ usageMessage progName

isIni :: String -> Bool
isIni filePath = drop (length filePath - 4) filePath == ".ini"

parseIniDirectory :: String -> IO ()
parseIniDirectory dir = do
  files <- listDirectory dir
  setCurrentDirectory dir
  let fileNames = filter isIni files
  fileContents <- sequence $ map readFile fileNames
  let namesAndContents = zip fileNames fileContents
  let m = foldr processEntry M.empty namesAndContents
  print m
  where
    processEntry (fileName, fileContents) m =
      M.insert fileName (maybeSuccess $ parseIniString fileContents) m

main :: IO ()
main = do
  args <- getArgs
  if length args == 1 then
    parseIniDirectory $ head args
  else
    printUsageMessage
{-# LANGUAGE NamedFieldPuns #-}
module Morra where

import System.Random (randomRIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Data.Char (toLower)
import Control.Monad (forever)
import System.Console.ANSI (clearScreen)

type Guess = Int
type Throw = Int
data GameState =
  GS {
       p1Score :: Int,
       p1Plays :: [(Guess, Throw)],
       p2Score :: Int,
       p2Plays :: [(Guess, Throw)]
     }

instance Show GameState where
  show (GS { p1Score, p2Score }) =
    "Player 1: " ++ show p1Score ++
    "\nPlayer 2: " ++ show p2Score

type GameStateT = StateT GameState IO

initialState = GS { p1Score = 0, p1Plays = [], p2Score = 0, p2Plays = [] }

lowerCase :: String -> String
lowerCase = map toLower

computerPlay :: GameStateT (Int, Int)
computerPlay = lift $ do
  throw <- randomRIO (0, 5)
  guess <- randomRIO (throw, 10)
  return (throw, guess)

getInt :: IO Int
getInt = getLine >>= pure . read

putStrLn' s = lift $ putStrLn s
getLine' = lift getLine
getInt' = lift getInt

calcScoresAndPromptToPlayAgain :: Bool -> GameStateT ()
calcScoresAndPromptToPlayAgain p1Won = do
  gs <- get
  let p1s = p1Score gs
  let p2s = p2Score gs
  let newState =
        gs {
          p1Score = (if p1Won then p1s + 1 else p1s),
          p2Score = (if p1Won then p2s else p2s + 1)
        }
  putStrLn' "\nThe scores so far..."
  putStrLn' (show newState)
  putStrLn' "\nPlay again? (Y/n)"
  answer <- getLine'
  if (lowerCase answer == "y" || answer == "") then
    put newState
  else
    fail "Game over."

runPvPGame :: GameStateT ()
runPvPGame =
  forever $ do
    lift clearScreen
    putStrLn' "PLAYER 1: What's your guess?"
    p1g <- getInt'
    lift clearScreen
    putStrLn' "PLAYER 2: What's your guess?"
    p2g <- getInt'
    lift clearScreen
    putStrLn' "PLAYER 1: Show your hand!"
    p1t <- getInt'
    putStrLn' "PLAYER 2: Show your hand!"
    p2t <- getInt'
    let total = p1t + p2t
    let p1Won = abs (total - p1g) <= abs (total - p2g)
    putStrLn' "--------------"
    putStrLn' $ "Player 1 threw " ++ show p1t ++ " and guessed " ++ show p1g
    putStrLn' $ "Player 2 threw " ++ show p2t ++ " and guessed " ++ show p2g
    putStrLn' "--------------"
    putStrLn' (if p1Won then "PLAYER 1 WINS!" else "PLAYER 2 WINS!")
    calcScoresAndPromptToPlayAgain p1Won

runAIGame :: GameStateT ()
runAIGame =
  -- The key piece of information here...
  -- you can pass a StateT action to forever
  -- without passing a value first because
  -- forever :: m a -> m b
  -- so in our case, it's specialized to
  -- forever :: StateT Scores IO () -> StateT Scores IO ()
  -- Remember, StateT actions only "run" when you pass a value to them
  -- using something like evalStateT or runStateT
  forever $ do
    putStrLn' "What's your guess?"
    hg <- getInt'
    putStrLn' "Throw!"
    ht <- getInt'
    (ct, cg) <- computerPlay
    putStrLn' "--------------"
    putStrLn' $ "You threw " ++ show ht ++ " and guessed " ++ show hg
    putStrLn' $ "Player 2 threw " ++ show ct ++ " and guessed " ++ show cg
    putStrLn' "--------------"
    let total = ht + ct
    let humanWon = abs (total - hg) <= abs (total - cg)
    putStrLn' (if humanWon then "You win this round!" else "Sorry, Player 2 won this one.")
    modify (\gs -> gs { p1Plays = (hg, ht):p1Plays gs, p2Plays = (cg, ct):p2Plays gs })
    calcScoresAndPromptToPlayAgain humanWon

play :: IO ()
play = do
  putStrLn "Welcome! Let's play Morra. Choose an option:\n \
            \ 1) 1-player game against AI\n \
            \ 2) 2-player game"
  mode <- getInt
  -- in a real application, we would use readMaybe to be safe
  case mode of
    1 -> evalStateT runAIGame initialState
    2 -> evalStateT runPvPGame initialState
    _ -> putStrLn "Goldarnit! You didn't enter 1 or 2! What am I gonna do with you..."
  
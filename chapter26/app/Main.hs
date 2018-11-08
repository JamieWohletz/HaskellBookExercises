{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai (Response)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
  Config {
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty =
  ScottyT Text (ReaderT Config IO)
type Handler =
  ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = (M.insert k newCount m, newCount)
  where
    count = fromMaybe 0 $ M.lookup k m
    newCount = count + 1

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    prfx <- lift $ asks prefix
    cnts <- lift $ asks counts
    cntsMap <- liftIO $ readIORef cnts
    let key' = mappend prfx unprefixed
    let (newMap, newInt) = bumpBoomp key' cntsMap
    liftIO $ writeIORef cnts newMap
    html $
      mconcat [ "<h1>Success! Count was: "
              , TL.pack $ show newInt
              , "</h1>"
              ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter (TL.pack prefixArg)
      runR :: ReaderT Config IO Response -> IO Response
      runR =  (flip runReaderT) config
  scottyT 3000 runR app

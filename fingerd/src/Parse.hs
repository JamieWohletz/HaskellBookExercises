module Parse where

import Text.Trifecta
import Db (User, mkUser)
import Data.ByteString (ByteString)
import Data.Text (Text, pack)
import Control.Applicative ((<|>))

userField :: Parser Text
userField = pack <$> some (noneOf ";\b\t\n\r")

type Username = Text

data DbQuery =
  Put User |
  Del Username
  deriving Show

-- parses strings that look like this:
-- "put <username>;<shell>;<homeDirectory>;<realName>;<phone>"
putQueryParser :: Parser DbQuery
putQueryParser = do
  skipSome (symbol "put")
  [username, shell, homeDirectory, realName, phone] <- semiSep userField
  return . Put $ mkUser username shell homeDirectory realName phone

-- parses strings that look like this:
-- "del <username>"
delQueryParser :: Parser DbQuery
delQueryParser = do
  skipSome (symbol "del")
  username <- userField
  return $ Del username

dbQueryParser :: Parser DbQuery
dbQueryParser = delQueryParser <|> putQueryParser

parseDbQuery :: ByteString -> Result DbQuery
parseDbQuery bs = parseByteString dbQueryParser mempty bs
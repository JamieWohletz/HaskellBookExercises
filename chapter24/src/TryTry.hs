module TryTry where

import Control.Applicative
import Text.Trifecta
import Text.Fractions

type FractionOrDecimal =
  Either Rational Double

parseDecimalOrFraction :: Parser FractionOrDecimal
parseDecimalOrFraction =
  (try (Left <$> parseFraction)) <|> (Right <$> double)

x = "12/53"
y = "53.2315"
z = "hahaha"

test = do
  let p f i = parseString f mempty i
  print $ p parseDecimalOrFraction x
  print $ p parseDecimalOrFraction y
  print $ p parseDecimalOrFraction z
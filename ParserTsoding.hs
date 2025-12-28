module ParserTsoding where

import Control.Applicative
import Data.Char

data JsonValue = JsonNull
               | JsonBool Bool
               | JsonNumber Int
               | JsonString String
               | JsonArray [JsonValue]
               | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

newtype Parser a = Parser {
  runParser :: String -> Maybe (String, a)
}

charP :: Char -> Parser Char
charP x = Parser f
  where
    f [] = Nothing
    f (y:ys)
      | y == x = Just (ys, y)
      | otherwise = Nothing

stringP :: String -> Parser String
stringP = traverse charP  -- equivalent to sequenceA $ map charP xs

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \inp -> let (val, rest) = span f inp in Just (rest, val)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \inp ->
  case p inp of
    Just (_, []) -> Nothing
    res -> res

stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/='"') <* charP '"'

ws :: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy ps pe = (:) <$> pe <*> many (ps *> pe) <|> pure []

optionalPrefix :: Parser String -> Parser String
optionalPrefix p = p <|> Parser (\inp -> Just (inp, ""))

optionalP :: Parser String -> Parser (String -> String)
optionalP p = (++) <$> optionalPrefix p

instance Functor Parser where
  fmap f (Parser fn) = Parser $ \inp ->
    case fn inp of
      Just (rest, v) -> Just (rest, f v)
      Nothing -> Nothing

instance Applicative Parser where
  pure x = Parser $ \inp -> Just (inp, x)
  (Parser p1) <*> (Parser p2) = Parser $ \inp ->
    do
      (rest1, f) <- p1 inp
      (rest2, a) <- p2 rest1
      return (rest2, f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \inp ->
    p1 inp <|> p2 inp

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringP "null"

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false")
  where f "true" = JsonBool True
        f "false" = JsonBool False

jsonNumber :: Parser JsonValue
jsonNumber = f <$> (optionalP (stringP "-") <*> notNull (spanP isDigit))
  where f ds = JsonNumber $ read ds

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonArray :: Parser JsonValue
jsonArray = JsonArray<$> (charP '[' *> ws *> elements <* ws <* charP ']')
  where
    elements = sepBy sep jsonValue
    sep = ws *> charP ',' <* ws

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$>
             (charP '{' *> ws *>
             sepBy (ws *> charP ',' <* ws) pair
             <* ws <* charP '}')

pair :: Parser (String, JsonValue)
pair = (\k _ v -> (k, v)) <$> (stringLiteral <|> stringLiteral)
                          <*> (ws *> charP ':' <* ws)
                          <*> jsonValue

jsonValue :: Parser JsonValue
jsonValue = jsonNull
        <|> jsonBool
        <|> jsonNumber
        <|> jsonString
        <|> jsonArray
        <|> jsonObject

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
  input <- readFile fileName
  return (snd <$> runParser parser input)


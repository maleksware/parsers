module Lambda where

import Data.Char
import Control.Applicative

type Variable = String

data LExpr = Var Variable
           | App LExpr LExpr
           | Abs Variable LExpr
  deriving (Eq, Show)

newtype Parser a = Parser {
  runParser :: String -> Maybe (a, String)
}

-- Easier to type
lambda :: Char
lambda = '&'

instance Functor Parser where
  fmap f (Parser p) = Parser $ \inp -> do
    (r, rest) <- p inp
    return (f r, rest)

instance Applicative Parser where
  pure c = Parser $ \inp -> Just (c, inp)
  Parser f <*> Parser p = Parser $ \inp -> do
    (fn, rest) <- f inp
    (p', rest') <- p rest
    return (fn p', rest')

instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser u <|> Parser v = Parser $ \inp -> u inp <|> v inp

instance Monad Parser where
  return = pure
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser p >>= f = Parser $ \inp -> case p inp of
    Just (r, rest) -> let (Parser f') = f r in f' inp
    Nothing -> Nothing

satisfy :: (Char -> Bool) -> Parser Char
satisfy pred = Parser $ \inp ->
  case inp of
    x:rest | pred x-> Just (x, rest)
           | otherwise -> Nothing
    _      -> Nothing

charP :: Char -> Parser Char
charP c = satisfy (==c)

sepBySome :: Parser a -> Parser b -> Parser [a]
sepBySome p sp = (:) <$> p <*> many (sp *> p)

-- It is unclear how to efficiently parse trailing separators as opposed to leading.
-- sepBy p sp = many (p <* sp) <|> (++) <$> many (p <* sp) <*> (pure <$> p) <|> pure []
-- is an implementation that should do the trick, but it is inefficient in many ways
-- (the most obvious being the ++)
sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sp = sepBySome p sp <|> pure []


stringP :: String -> Parser String
stringP = traverse (\c -> satisfy (==c))

term :: Parser LExpr
term = variable <|> abstraction <|> grouping

ws :: Parser String
ws = many (satisfy isSpace)

literal :: Parser Variable
literal = Parser $ \inp ->
  let (xs, rest) = span (\c -> isDigit c || isAlpha c) inp
  in (if null xs then Nothing else Just (xs, rest))

variable :: Parser LExpr
variable = Var <$> literal

abstraction :: Parser LExpr
abstraction = (\_ v _ e -> Abs v e) <$> charP lambda <* ws
                                    <*> literal <* ws
                                    <*> charP '.' <* ws
                                    <*> expression

application :: Parser LExpr
application = foldl1 App <$> terms
  where terms = sepBySome term (some $ satisfy isSpace)

grouping :: Parser LExpr
grouping = charP '(' *> expression <* charP ')'

expression :: Parser LExpr
expression = application
-- Can be expression = application <|> term,
-- but if application fails to parse more than 1 term,
-- foldl1 returns the element without injecting App,
-- so the check for the individual term is redundant.

formatExpr :: LExpr -> String
formatExpr expr = case expr of
  Var v -> v
  Abs v rest -> "& " ++ v ++ ".[" ++ formatExpr rest ++ "]"
  App f a -> "({" ++ formatExpr f ++ "} {" ++ formatExpr a ++ "})"


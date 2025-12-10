module Parser where

import Control.Applicative
import Data.Char


newtype Parser a = Parser {
    runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (input', x) <- p input
    Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do
    (input', f) <- p1 input
    (input'', x) <- p2 input'
    Just (input'', f x)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    p1 input <|> p2 input

instance Monad Parser where
  p >>= k = Parser $ \input -> case runParser p input of
    Nothing -> Nothing
    Just (input', x) -> runParser (k x) input'


-- separates by parsing a delimiter and then parsing all elements and constructing a list
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []  -- if fail, empty list


-- same as isSpace, but including the new line char '\' that .md files use
space :: Char -> Bool
space c = case c of
  ' '  -> True
  '\t' -> True
  '\n' -> True
  '\r' -> True
  '\\' -> True
  _    -> False


ws :: Parser String
ws = spanP space


-- bit misleading as this parses any non whitespace char
alphanumeric :: Parser String
alphanumeric = spanP $ not . space


-- parses variable names
codeString :: Parser String
codeString = spanP isCodeChar
  where
    isCodeChar c = if isAlphaNum c
                   then True 
                   else case c of
      '_' -> True
      _   -> False


-- parses until it hits a new line
notNewLine :: Parser String
notNewLine = spanP (/='\n')


-- parses looking for a specific char
charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y:ys) | y == x = Just (ys, x)
             | otherwise = Nothing
    f [] = Nothing


-- parses looking for a specific string
-- turns [Parser Char] -> Parser [Char] using sequenceA
stringP :: String -> Parser String
stringP = sequenceA . map charP


-- parses until a predicate is true
spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
  let (token, rest) = span f input
    in Just (rest, token)

module MDParser where

-- the main portion of how this parser works was derived from Tsoding's JSON parser
-- https://github.com/tsoding/haskell-json.git
--
-- Converted to Markdown Parser by Lewis Vick 2025
-- https://github.com/vl35c

import Data.Char
import Control.Applicative


data Markdown = Header String
              | HeaderDescription String
              | Subheader String
              | CodeSnippet String
              | CodeDescription String
              | HorizontalBreak
  deriving (Show)


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


alphanumeric :: Parser String
alphanumeric = spanP $ not . space


notNewLine :: Parser String
notNewLine = spanP (/='\n')


charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y:ys) | y == x = Just (ys, x)
             | otherwise = Nothing
    f [] = Nothing


stringP :: String -> Parser String
stringP = sequenceA . map charP


spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input ->
  let (token, rest) = span f input
    in Just (rest, token)


startsP :: String -> Parser String
startsP xs = Parser $ \input -> do
  (input', match) <- runParser (stringP xs) input
  (input'', rest) <- runParser (alphanumeric) input'
  Just (input'', match ++ rest)


codeLiteral :: Parser String
codeLiteral = spanP (/= '`')


parseHeader :: Parser Markdown
parseHeader = Header <$> (ws *> stringP "# " *> alphanumeric <* ws <* notNewLine <* ws)

parseHeaderDescription :: Parser Markdown
parseHeaderDescription = HeaderDescription <$> (ws *> notNewLine <* ws)


parseSubheader :: Parser Markdown
parseSubheader = Subheader <$> (ws *> stringP "### " *> alphanumeric <* notNewLine <* ws)


parseCodeSnippet :: Parser Markdown
parseCodeSnippet = CodeSnippet <$> (ws *> charP '`' *> codeLiteral <* charP '`' <* ws)


parseDescription :: Parser Markdown
parseDescription = CodeDescription <$> (ws *> stringP "- " *> notNewLine <* ws)


parseHorizontalBreak :: Parser Markdown
parseHorizontalBreak = (\_ -> HorizontalBreak) <$> (ws *> stringP "---" <* ws)


parseMarkdown :: Parser Markdown
parseMarkdown = parseHeader <|> parseSubheader <|> parseCodeSnippet <|> parseDescription <|> parseHorizontalBreak


parse :: String -> [Markdown]
parse input = case output of
  Just (rest, token) -> token : (parse rest)
  Nothing            -> []
  where output = runParser parseMarkdown input

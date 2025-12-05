module MDParser where

-- the main portion of how this parser works was derived from Tsoding's JSON parser
-- https://github.com/tsoding/haskell-json.git
--
-- Converted to Markdown Parser by Lewis Vick 2025
-- https://github.com/vl35c

import Data.Char
import Control.Applicative


data Markdown = Class String String
              | Subclass String String String
              | Subheader String
              | CodeSnippet String String
              | HorizontalBreak

instance Show Markdown where
  show (Class className description)               = "\"Class<s>" ++ className ++ "<s>"  ++ description ++ "\""
  show (Subclass className superClass description) = "\"Subclass<s>" ++ className ++ "<s>" ++ superClass ++ "<s>" ++ description ++ "\""
  show (Subheader subheader)                       = "\"Subheader<s>" ++ subheader ++ "\""
  show (CodeSnippet code description)              = "\"CodeSnippet<s>" ++ code ++ "<s>" ++ description ++ "\""
  show HorizontalBreak                             = "\"HB\""


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


-- parses .md code snippets which are between '`' characters
codeLiteral :: Parser String
codeLiteral = spanP (/= '`')


-- parses the # HEADER in a .md file, and then gets the next line as the description
parseClass :: Parser Markdown
parseClass = do
   _ <- (ws <* stringP "# ")
   className <- (alphanumeric <* ws)
   description <- (notNewLine <* ws)
   Parser $ \input -> Just (input, Class className description)


parseSubClass :: Parser Markdown
parseSubClass = do
  _ <- (ws <* stringP "# ")
  className <- ((spanP isAlpha) <* ws)
  superClass <- (charP '(' *> (spanP isAlpha) <* charP ')' <* ws)
  description <- (notNewLine <* ws)
  Parser $ \input -> Just (input, Subclass className description superClass)



-- parses out the subheaders - in parsnip these are ### PROPERTIES and ### METHODS
parseSubheader :: Parser Markdown
parseSubheader = Subheader <$> (ws *> stringP "### " *> alphanumeric <* notNewLine <* ws)


-- parses code snippets and their descriptions
parseCodeSnippet :: Parser Markdown
parseCodeSnippet = do
  _ <- (ws *> charP '`')
  code <- codeLiteral
  _ <- (charP '`' <* ws <* stringP "- ")
  description <- (notNewLine <* ws)
  Parser $ \input -> Just (input, CodeSnippet code $ takeWhile (/='\\') description)


-- parses horizontal breaks
parseHorizontalBreak :: Parser Markdown
parseHorizontalBreak = (\_ -> HorizontalBreak) <$> (ws *> stringP "---" <* ws)


-- complete .md parser
-- note this is specific for parsnip and does not parse every feature of a .md file
parseMarkdown :: Parser Markdown
parseMarkdown = parseSubClass
            <|> parseClass
            <|> parseSubheader 
            <|> parseCodeSnippet 
            <|> parseHorizontalBreak 


-- the parse function called in io
parse :: String -> [Markdown]
parse input = case output of
  Just (rest, token) -> token : (parse rest)
  Nothing            -> []
  where output = runParser parseMarkdown input

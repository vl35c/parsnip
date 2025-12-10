module MDParser where

-- the main portion of how this parser works was derived from Tsoding's JSON parser
-- https://github.com/tsoding/haskell-json.git
--
-- Converted to Markdown Parser by Lewis Vick 2025
-- https://github.com/vl35c

import Data.Char
import Control.Applicative
import Parser


data Markdown = Class String String
              | Subclass String String String
              | Subheader String
              | CodeSnippet Markdown String
              | Code String
              | Function String
              | HorizontalBreak

instance Show Markdown where
  show (Class className description)               = "\"Class<s>" ++ className ++ "<s>"  ++ description ++ "\""
  show (Subclass className superClass description) = "\"Subclass<s>" ++ className ++ "<s>" ++ superClass ++ "<s>" ++ description ++ "\""
  show (Subheader subheader)                       = "\"Subheader<s>" ++ subheader ++ "\""
  show (CodeSnippet code description)              = "\"CodeSnippet<s>" ++ show code ++ "<s>" ++ description ++ "\""
  show HorizontalBreak                             = "\"HB\""
  show (Code code)                                 = code
  show (Function code)                             = code


-- parses .md code snippets which are between '`' characters
codeLiteral :: Parser Markdown
codeLiteral = Code <$> ((spanP (/= '`')))


codeString :: Parser String
codeString = spanP isCodeChar
  where
    isCodeChar c = if isAlpha c 
                   then True 
                   else case c of
      '_' -> True
      _   -> False


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


parseFunction :: Parser Markdown
parseFunction = do
  name <- codeString <* ws <* charP '('
  _ <- spanP (/=')') <* charP ')' <* ws
  returnType <- (stringP "->" *> ws *> spanP (/='`') <* ws) <|> pure ""
  Parser $ \input -> Just (input, Function name)


-- parses code snippets and their descriptions
parseCodeSnippet :: Parser Markdown
parseCodeSnippet = do
  _ <- (ws <* charP '`')
  code <- parseFunction <|> codeLiteral
  _ <- (stringP "` - " <* ws)
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
parse_md :: String -> [Markdown]
parse_md input = case output of
  Just (rest, token) -> token : (parse_md rest)
  Nothing            -> []
  where output = runParser parseMarkdown input

module PythonParser where

import Parser
import Data.Char
import Control.Applicative

data Python = Class String
            | Param String String
            | Function String [Python]
            | FunctionCall String
            | Property String String String
            | For String String
            | ImportFrom String String
            | Comment String
  deriving (Show)


codeString :: Parser String
codeString = spanP isCodeChar
  where
    isCodeChar c = if isAlpha c 
                   then True 
                   else case c of
      '_' -> True
      _   -> False


callString :: Parser String
callString = spanP isCallChar
  where
    isCallChar c = if isAlpha c
                   then True
                   else case c of
      '_' -> True
      '.' -> True
      _   -> False


sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []


parseParam :: Parser Python
parseParam = do
  name <- codeString <* ws
  varType <- (charP ':' *> ws *> codeString <* ws) <|> pure ""
  Parser $ \input -> Just (input, Param name varType)


parseImportFrom :: Parser Python
parseImportFrom = do
  from <- stringP "from" *> ws *> (spanP isAlpha) <* ws
  import' <- ws *> stringP "import" *> ws *> (spanP isAlpha) <* ws
  Parser $ \input -> Just (input, ImportFrom from import')


parseClass :: Parser Python
parseClass = Class <$> (stringP "class" *> ws *> (spanP isAlpha) <* ws <* charP ':' <* ws)


parseFunction :: Parser Python
parseFunction = do
  _ <- stringP "def" <* ws
  name <- (spanP (/='('))
  params <- charP '(' *> ws *> (sepBy (ws *> charP ',' <* ws) (parsePython <|> parseParam)) <* ws <* charP ')'
  rest <- notNewLine <* ws
  Parser $ \input -> Just (input, Function name params)


parseFunctionCall :: Parser Python
parseFunctionCall = FunctionCall <$> (callString <* charP '(' <* notNewLine <* ws)


parseProperty :: Parser Python
parseProperty = do
  name <- stringP "self." *> (codeString) <* ws
  varType <- (charP ':' *> ws *> (spanP (/='=')) <* ws) <|> pure ""
  value <- stringP "=" *> ws *> notNewLine <* ws
  Parser $ \input -> Just (input, Property name varType value)


parseFor :: Parser Python
parseFor = do
  item <- stringP "for" *> ws *> (spanP isAlpha) <* ws
  iterator <- notNewLine <* ws
  Parser $ \input -> Just (input, For item iterator)


parseComment :: Parser Python
parseComment = Comment <$> (charP '#' *> ws *> notNewLine <* ws)


parsePython :: Parser Python
parsePython = parseClass <|>
              parseFunction <|>
              parseProperty <|>
              parseFor <|>
              parseImportFrom <|>
              parseComment <|>
              parseFunctionCall


parse_py :: String -> [Python]
parse_py input = case output of
  Just (rest, token) -> token : (parse_py rest)
  Nothing            -> []
  where output = runParser parsePython input

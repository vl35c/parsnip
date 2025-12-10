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

instance Show Python where
  show (Class name) = "\"Class<s>" ++ name ++ "\""
  show (Param name varType) = "Param<p>" ++ name ++ "<p>" ++ varType
  show (Function name params) = "\"Function<s>" ++ name ++ "<s>" ++ show params ++ "\""
  show (FunctionCall name) = "\"FunctionCall<s>" ++ name ++ "\""
  show (Property name varType value) = "\"Property<s>" ++ name ++ "<s>" ++ varType ++ "<s>" ++ value ++ "\""
  show (For alias iterator) = "\"For<s>" ++ alias ++ "<s>" ++ iterator ++ "\""
  show (ImportFrom library package) = "\"ImportFrom<s>" ++ library ++ "<s>" ++ package ++ "\""
  show (Comment comment) = "\"Comment<s>" ++ comment ++ "\""



-- parses called functions, i.e. variables names joined by .
callString :: Parser String
callString = spanP isCallChar
  where
    isCallChar c = if isAlpha c
                   then True
                   else case c of
      '_' -> True
      '.' -> True
      _   -> False


-- parses a function parameter
parseParam :: Parser Python
parseParam = do
  name <- codeString <* ws
  varType <- (charP ':' *> ws *> codeString <* ws) <|> pure "unset"
  Parser $ \input -> Just (input, Param name varType)


-- parses import x from y
parseImportFrom :: Parser Python
parseImportFrom = do
  from <- stringP "from" *> ws *> (spanP isAlpha) <* ws
  import' <- ws *> stringP "import" *> ws *> (spanP isAlpha) <* ws
  Parser $ \input -> Just (input, ImportFrom from import')


-- parses class ...
parseClass :: Parser Python
parseClass = Class <$> (stringP "class" *> ws *> (spanP isAlpha) <* ws <* charP ':' <* ws)


-- parses def ...
parseFunction :: Parser Python
parseFunction = do
  _ <- stringP "def" <* ws
  name <- (spanP (/='('))
  params <- charP '(' *> ws *> (sepBy (ws *> charP ',' <* ws) (parsePython <|> parseParam)) <* ws <* charP ')'
  rest <- notNewLine <* ws
  Parser $ \input -> Just (input, Function name params)


-- parses a function call
parseFunctionCall :: Parser Python
parseFunctionCall = FunctionCall <$> (callString <* charP '(' <* notNewLine <* ws)


-- parses a property in an object
parseProperty :: Parser Python
parseProperty = do
  name <- stringP "self." *> (codeString) <* ws
  varType <- (charP ':' *> ws *> (spanP (/='=')) <* ws) <|> pure ""
  value <- stringP "=" *> ws *> notNewLine <* ws
  Parser $ \input -> Just (input, Property name varType value)


-- parses a for loop - probably not solid code
parseFor :: Parser Python
parseFor = do
  item <- stringP "for" *> ws *> (spanP isAlpha) <* ws
  iterator <- notNewLine <* ws
  Parser $ \input -> Just (input, For item iterator)


-- parses comment 
parseComment :: Parser Python
parseComment = Comment <$> (charP '#' *> ws *> notNewLine <* ws)


parsePlaceholder :: Parser Python
parsePlaceholder =  (\_ -> Comment "...") <$> (ws *> stringP "..." <* ws)


-- complete parser
parsePython :: Parser Python
parsePython = parseClass <|>
              parseFunction <|>
              parseProperty <|>
              parseFor <|>
              parseImportFrom <|>
              parseComment <|>
              parseFunctionCall <|>
              parsePlaceholder


-- io calls this
parse_py :: String -> [Python]
parse_py input = case output of
  Just (rest, token) -> token : (parse_py rest)
  Nothing            -> []
  where output = runParser parsePython input

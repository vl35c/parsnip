module PythonParser where

import Parser
import Data.Char
import Control.Applicative

data Python = Class String
            | Function String
            | FunctionCall String
            | Property String
            | For String String
            | ImportFrom String String
            | Comment String
  deriving (Show)


parseImportFrom :: Parser Python
parseImportFrom = do
  from <- stringP "from" *> ws *> (spanP isAlpha) <* ws
  import' <- ws *> stringP "import" *> ws *> (spanP isAlpha) <* ws
  Parser $ \input -> Just (input, ImportFrom from import')


parseClass :: Parser Python
parseClass = Class <$> (stringP "class" *> ws *> (spanP isAlpha) <* ws <* charP ':' <* ws)


parseFunction :: Parser Python
parseFunction = Function <$> (stringP "def" *> ws *> (spanP (/= ')')) <* stringP "):" <* ws)


parseFunctionCall :: Parser Python
parseFunctionCall = FunctionCall <$> ((spanP (/='(')) <* charP '(' <* (spanP (/=')')) <* charP ')' <* ws)


parseProperty :: Parser Python
parseProperty = Property <$> (stringP "self." *> notNewLine <* ws)


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

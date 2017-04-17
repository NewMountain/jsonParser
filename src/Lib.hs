module Lib
    ( someFunc
    ) where

import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Applicative


data JSONValue = 
    B Bool | S String | A [JSONValue] | O [(String, JSONValue)]
    deriving (Show)

jsonValue :: Parser JSONValue 
jsonValue = jsonBool <|> jsonStringLiteral <|> jsonArray <|> jsonObject

-- Whitespace
--ws :: Parser String
--ws = many (oneOf " \t\n")

-- Booleans
boolTrue :: Parser Bool
boolTrue = string "true" *> pure True

boolFalse :: Parser Bool
boolFalse = string "false" *> pure False

bool :: Parser Bool
bool = boolTrue <|> boolFalse

jsonBool :: Parser JSONValue
jsonBool = B <$> bool


-- Strings
stringLiteral :: Parser String
stringLiteral = 
    char '"' *> many (noneOf "\"") <* char '"'

jsonStringLiteral :: Parser JSONValue
jsonStringLiteral = S <$> stringLiteral


-- Objects
objectEntry :: Parser (String, JSONValue)
objectEntry = do
    key <- stringLiteral
    _ <- char ':'
    value <- jsonValue
    return (key, value)
    
--jsonObject :: Parser (String, JSONValue)
jsonObject = 
    O <$> (char '{' *> (objectEntry `sepBy` char ',') <* char '}')


-- Arrays
array :: Parser [JSONValue]
array = 
    char '[' *> jsonValue `sepBy` char ',' <* char ']'
    
jsonArray :: Parser JSONValue
jsonArray = A <$> array




someFunc :: IO ()
someFunc = do
    print $ parse jsonValue "test" "true"

    print $ parse jsonValue "test" "false"

    print $parse jsonValue "test" "\"Hello world!\""

    -- Note does not handle spaces gracefully right now, but error handling
    -- is quite good out of the box
    print $ parse jsonValue "test" "[\"Hello\",\"Goodbye\",true,false,true]"

    print $ parse jsonValue "test" "{\"name\":\"Chris\",\"likes beer\":true}"

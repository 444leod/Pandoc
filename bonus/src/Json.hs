{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- JsonParser
-}

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Json
    ( JsonValue(..)
    , parseJsonValue
    , printJson
) where

import ParserLib

--- JSON PARSING FUNCTIONS ---

{- | JsonValue
    Represents a JSON value
-}
data JsonValue =
    Null |
    Boolean Bool |
    Number Int |
    JString String |
    JArray [JsonValue] |
    JObject [(String, JsonValue)]
    deriving (Show)

{- | parseNull function
    Parse a null value
    Return a Null JsonValue or Nothing
-}
parseNull :: Parser JsonValue
parseNull = Parser $ \str ->
    case str of
        ('n':'u':'l':'l':rest) -> Just (Null, rest)
        _ -> Nothing

{- | parseBoolean function
    Parse a boolean value
    Return a Boolean JsonValue or Nothing
-}
parseBoolean :: Parser JsonValue
parseBoolean = Parser $ \str ->
    case str of
        ('t':'r':'u':'e':rest) -> Just (Boolean True, rest)
        ('f':'a':'l':'s':'e':rest) -> Just (Boolean False, rest)
        _ -> Nothing

{- | parseNumber function
    Parse a number value
    Return a Number JsonValue or Nothing
-}
parseNumber :: Parser JsonValue
parseNumber = Parser $ \str ->
    case runParser parseInt str of
        Just (result, rest) -> Just (Number result, rest)
        Nothing -> Nothing

{- | parseJString function
    Parse a string value
    Return a JString JsonValue or Nothing
-}
parseJString :: Parser JsonValue
parseJString = Parser $ \str ->
    case str of
        ('"':'"':rest) -> Just (JString "", rest)
        ('"':rest) -> do
            (result, rest1) <- runParser (parseSome (parseExceptChar '"')) rest
            (_, rest2) <- runParser (parseChar '"') rest1
            return (JString result, rest2)
        _ -> Nothing

{- | parseJArray function
    Parse an array value
    Return a JArray JsonValue or Nothing
-}
parseJArray :: Parser JsonValue
parseJArray = Parser $ \str ->
    case str of
        ('[':rest) -> runParser (removePadding
            *> fmap JArray (getJArray [])) rest
        _ -> Nothing

{- | getJArray function
    Get an array from a string without the first bracket
    Return an array of JsonValue or Nothing
-}
getJArray :: [JsonValue] -> Parser [JsonValue]
getJArray arr = Parser $ \str ->
    case str of
        (']':rest) -> Just (arr, rest)
        _ -> do
            (result, rest) <- runParser (removePadding *> parseJsonValue) str
            (_, rest2) <- runParser (removePadding *> parseCommaJArray) rest
            runParser (getJArray (arr ++ [result])) rest2

{- | parseCommaJArray function
    Used to parse a comma in an JArray
    Return a comma or an empty string or Nothing
-}
parseCommaJArray :: Parser String
parseCommaJArray = Parser $ \str ->
  case str of
    (',':']':_) -> Nothing
    (']':_) -> Just ("", str)
    (x:xs)
        | x == ',' -> Just (",", xs)
        | otherwise -> Nothing
    _ -> Just ("", str)

{- | parseJObject function
    Parse an object value
    Return a JObject JsonValue or Nothing
-}
parseJObject :: Parser JsonValue
parseJObject = Parser $ \str ->
    case str of
        ('{':rest) -> runParser (removePadding
            *> fmap JObject (getJObject [])) rest
        _ -> Nothing

{- | getJObject function
    Get an object from a string without the first curly bracket
    Return an array of (String, JsonValue) or Nothing
-}
getJObject :: [(String, JsonValue)] -> Parser [(String, JsonValue)]
getJObject arr = Parser $ \str -> case str of
    ('}':rest) -> Just (arr, rest)
    _ -> do (result, rest) <- runParser (removePadding *> parseString) str
            (_, rest2) <- runParser (removePadding *> parseChar ':') rest
            (result2, rest3) <- runParser (removePadding
                *> parseJsonValue) rest2
            (_, rest4) <- runParser (removePadding *> parseCommaJObject) rest3
            runParser (getJObject (arr ++ [(result, result2)])) rest4

{- | parseCommaJObject function
    Used to parse a comma in an JObject
    Return a comma or an empty string or Nothing
-}
parseCommaJObject :: Parser String
parseCommaJObject = Parser $ \str ->
  case str of
    (',':'}':_) -> Nothing
    ('}':_) -> Just ("", str)
    (x:xs)
        | x == ',' -> Just (",", xs)
        | otherwise -> Nothing
    _ -> Just ("", str)

{- | parseJsonValue function
    Parse a JSON value from a string
    Return a JsonValue or Nothing
-}
parseJsonValue :: Parser JsonValue
parseJsonValue = removePadding *> (
    parseNull <|>
    parseBoolean <|>
    parseNumber <|>
    parseJString  <|>
    parseJArray <|>
    parseJObject
    )

--- JSON PRINTING FUNCTIONS ---

{- | printJArray function
    Return a string representation of the array
-}
printJArray :: [JsonValue] -> String
printJArray [] = ""
printJArray [x] = printJson x
printJArray (x:xs) = printJson x ++ "," ++ printJArray xs

{- | printJObject function
    Return a string representation of the object
-}
printJObject :: [(String, JsonValue)] -> String
printJObject [] = ""
printJObject [(key, value)] = show key ++ ":" ++ printJson value
printJObject ((key, value):xs) =
    show key ++ ":" ++ printJson value ++ ",\n" ++ printJObject xs

{- | printJson function
    Return a string representation of the JSON value
-}
printJson :: JsonValue -> String
printJson Null = "null"
printJson (Boolean True) = "true"
printJson (Boolean False) = "false"
printJson (Number n) = show n
printJson (JString str) = show str
printJson (JArray arr) = "[" ++ printJArray arr ++ "]"
printJson (JObject obj) = "{" ++ printJObject obj ++ "}"

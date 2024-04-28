{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- Caclulator
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use second" #-}

module Calculator(
    expr,
    launchCalculator
) where

import ParserLib
import System.IO
import Debug.Trace
import Launcher(getFileContents, writeFileContents)
import Json

{- | Variables
    Represents a list of variables
-}
newtype Variables = Variables [(String, Int)] deriving (Show)


{- | expr
    Parse an expression and return the result
-}
expr :: Variables -> Parser Int
expr vars = Parser $ \str -> do
    (x, rest) <- runParser (removePadding *> term vars <* removePadding) str
    case rest of
        '+':rest' -> do
            (y, rest'') <- runParser (removePadding *> expr vars) rest'
            return (x + y, rest'')
        _ -> runParser (removePadding *> term vars) str

{- | term
    Parse a term and return the result
-}
term :: Variables -> Parser Int
term vars = Parser $ \str -> do
    (x, rest) <- runParser (removePadding *> factor vars <* removePadding) str
    case rest of
        '*':rest' -> do
            (y, rest'') <- runParser (removePadding *> term vars) rest'
            return (x * y, rest'')
        _ -> runParser (removePadding *> factor vars) str

{- | factor
    Parse a factor and return the result
-}
factor :: Variables -> Parser Int
factor vars = Parser $ \str ->
    runParser (removePadding *> (parseChar '(' *> removePadding *> expr vars
    <* removePadding <* parseChar ')' <|> parseUInt <|> parseVar vars)) str

{- | parseVar
    Parse a variable and return the value
-}
parseVar :: Variables -> Parser Int
parseVar (Variables vars) = Parser $ \str ->
    case runParser (removePadding *> parseUntilChars " \n\t+-*/()") str of
        Just (name, rest) -> case lookup name vars of
            Just x -> return (x, rest)
            Nothing -> Nothing
        Nothing -> Nothing
    
{- | lauchCalculator
    Launch the calculator with help message
-}
launchCalculator :: IO ()
launchCalculator =
    putStrLn "Enter an expression to calculate, type 'exit' to quit." >>
    putStrLn "Only handles UInt, Multiplication and Addition for now..." >>
    putStrLn "Define new variables using \"var = 'expression'\" notation." >>
    putStrLn "\"save 'fileName'\" to save the variables to a JSON file" >>
    putStrLn "\"load 'fileName'\" to load the variables from a JSON file" >>
    calcLoop (Variables [])

{- | calcLoop
    Loop to get the input and calculate the result
-}
calcLoop :: Variables -> IO ()
calcLoop vars = do
    putStr ">> "
    hFlush stdout
    input <- getLine
    handleInput input vars

{- | handleInput
    Handle the input and call the right function
-}
handleInput :: String -> Variables -> IO ()
handleInput "exit" _ = putStrLn "Goodbye"
handleInput ('s':'a':'v':'e':' ':xs) vars = case runParser (removePadding *>
    parseUntilChars " \t" <* removePadding) xs of
        Just (outfile, "") ->
            writeFileContents outfile (printJson (varsToJson vars)) >>
            calcLoop vars
        _ -> putStrLn "Invalid expression" >> calcLoop vars
handleInput ('l':'o':'a':'d':' ':xs) vars = case runParser (removePadding *>
    parseUntilChars " \t" <* removePadding) xs of
        Just (infile, "") -> do
            maybeJson <- getJsonFromFile infile
            case maybeJson of
                Just json -> calcLoop (loadVars json vars)
                _ -> putStrLn "Invalid file" >> calcLoop vars
        _ -> putStrLn "Invalid expression" >> calcLoop vars
handleInput exprToCalc vars =
    case runParser (removePadding *> expr vars <* removePadding) exprToCalc of
        Just (result, "") -> print result >> calcLoop vars
        _ -> handleNewVar exprToCalc vars

{- | handleNewVar
    Handle the new variable expression
-}
handleNewVar :: String -> Variables -> IO ()
handleNewVar exprToCalc (Variables vars) = case runParser ((,) <$>
    (removePadding *> expectNoSeparators "/*-+()" *> parseUntilChars " \n\t=")
    <*> (removePadding *> parseChar '=' *> removePadding *>
    expr (Variables vars) <* removePadding)) exprToCalc of
        Just ((name, result), "") -> calcLoop (Variables ((name, result):vars))
        _ -> putStrLn "Invalid expression" >> calcLoop (Variables vars)

{- | varsToJson
    Convert the variables to a JSON value
-}
varsToJson :: Variables -> JsonValue
varsToJson (Variables vars) =
    JObject [("Variables", JArray (map varToJson vars))]

{- | varToJson
    Convert a variable to a JSON value
-}
varToJson :: (String, Int) -> JsonValue
varToJson (name, value) =
    JObject [(name, Number value)]

{- | getJsonFromFile
    Get the JSON value from a file
-}
getJsonFromFile :: String -> IO (Maybe JsonValue)
getJsonFromFile infile = do
    fileContent <- getFileContents infile
    case fileContent of
        Just content -> return $ fst <$> runParser parseJsonValue content
        _ -> return Nothing

{- | loadVars
    Load the variables from a JSON value
-}
loadVars :: JsonValue -> Variables -> Variables
loadVars (JObject [("Variables", JArray vars)]) (Variables oldVars) =
    Variables (oldVars ++ map jsonToVar vars)
loadVars _ vars = vars

{- | jsonToVar
    Convert a JSON value to a variable
-}
jsonToVar :: JsonValue -> (String, Int)
jsonToVar (JObject [(name, Number value)]) = (name, value)
jsonToVar _ = ("", 0)


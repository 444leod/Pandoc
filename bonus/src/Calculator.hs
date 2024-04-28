{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- Caclulator
-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

module Calculator(
    expr,
    term,
    factor,
    launchCalculator
) where

import ParserLib
import System.IO
import Debug.Trace
import Launcher
import Json

newtype Variables = Variables [(String, Int)]

expr :: Variables -> Parser Int
expr vars = Parser $ \str -> do
    (x, rest) <- runParser (removePadding *> term vars <* removePadding) str
    case rest of
        '+':rest' -> do
            (y, rest'') <- runParser (removePadding *> expr vars) rest'
            return (x + y, rest'')
        _ -> runParser (removePadding *> term vars) str

term :: Variables -> Parser Int
term vars = Parser $ \str -> do
    (x, rest) <- runParser (removePadding *> factor vars <* removePadding) str
    case rest of
        '*':rest' -> do
            (y, rest'') <- runParser (removePadding *> term vars) rest'
            return (x * y, rest'')
        _ -> runParser (removePadding *> factor vars) str

factor :: Variables -> Parser Int
factor vars = Parser $ \str ->
    runParser (removePadding *> (parseChar '(' *> removePadding *> expr vars
    <* removePadding <* parseChar ')' <|> parseUInt <|> parseVar vars)) str
    
parseVar :: Variables -> Parser Int
parseVar (Variables vars) = Parser $ \str ->
    case runParser (removePadding *> parseUntilChars " \n\t+-*/()") str of
        Just (name, rest) -> case lookup name vars of
            Just x -> return (x, rest)
            Nothing -> Nothing
        Nothing -> Nothing

launchCalculator :: IO ()
launchCalculator =
    putStrLn "Enter an expression to calculate, type 'exit' to quit." >>
    putStrLn "Only handles UInt, Multiplication and Addition for now..." >>
    putStrLn "Define new variables using \"var = 'expression'\" notation.\n" >>
    calcLoop (Variables [])

calcLoop :: Variables -> IO ()
calcLoop vars = do
    putStr ">> "
    hFlush stdout
    input <- getLine
    handleInput input vars

handleInput :: String -> Variables -> IO ()
handleInput "exit" _ = putStrLn "Goodbye"
handleInput ('s':'a':'v':'e':' ':xs) vars = case runParser (removePadding *>
    parseUntilChars " \t" <* removePadding) xs of
        Just (outfile, "") ->
            writeFileContents outfile (printJson (varsToJson vars)) >>
            calcLoop vars
        _ -> putStrLn "Invalid expression" >> calcLoop vars
handleInput exprToCalc vars =
    case runParser (removePadding *> expr vars <* removePadding) exprToCalc of
        Just (result, "") -> print result >> calcLoop vars
        _ -> handleNewVar exprToCalc vars

handleNewVar :: String -> Variables -> IO ()
handleNewVar exprToCalc (Variables vars) = case runParser ((,) <$>
    (removePadding *> expectNoSeparators "/*-+()" *> parseUntilChars " \n\t=")
    <*> (removePadding *> parseChar '=' *> removePadding *>
    expr (Variables vars) <* removePadding)) exprToCalc of
        Just ((name, result), "") -> calcLoop (Variables ((name, result):vars))
        _ -> putStrLn "Invalid expression" >> calcLoop (Variables vars)

varsToJson :: Variables -> JsonValue
varsToJson (Variables vars) =
    JObject [("Variables", JArray (map varToJson vars))]

varToJson :: (String, Int) -> JsonValue
varToJson (name, value) =
    JObject [(name, Number value)]


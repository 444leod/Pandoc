{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- Main
-}

module Main(main) where

import System.Environment(getArgs)
import Config
import Calculator(launchCalculator)
import Launcher(launchFile)
import CSV

main :: IO ()
main = do
    args <- getArgs
    let conf = defaultConf
    let option = getOpts conf args
    bonusLauncher option

{- | bonusLauncher function
    Launch the program with the given configuration
-}
bonusLauncher :: Maybe Conf -> IO ()
bonusLauncher (Just (Conf _ _ _ _ (Just True) _)) = launchCalculator
bonusLauncher (Just (Conf (Just input) _ output _ _ (Just True))) =
    case output of
        Just output' -> launchCSV output' input
        Nothing -> launchCSV "" input
bonusLauncher option =
    validateConf option >>
    case option of
        Just opt -> launchFile (createVerifiedConf opt)
        Nothing -> myError "Error: invalid arguments"

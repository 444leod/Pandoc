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

main :: IO ()
main = do
    args <- getArgs
    let conf = defaultConf
    let option = getOpts conf args
    bonusLauncher option

bonusLauncher :: Maybe Conf -> IO ()
bonusLauncher (Just (Conf _ _ _ _ (Just True))) = launchCalculator
bonusLauncher option =
    validateConf option >>
    case option of
        Just opt -> launchFile (createVerifiedConf opt)
        Nothing -> myError "Error: invalid arguments"

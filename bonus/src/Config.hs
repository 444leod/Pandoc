{-
-- EPITECH PROJECT, 2024
-- Pandoc
-- File description:
-- Config
-}

module Config(
    Conf(..),
    VerifiedConf(..),
    ConfFormat(..),
    defaultConf,
    getOpts,
    validateConf,
    myError,
    createVerifiedConf
) where

import Data.Maybe(fromMaybe)
import System.Exit(exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, hPutStr, stderr)

data ConfFormat = JSON | XML | MARKDOWN | UNKNOWN deriving (Enum, Show)

{-  | Conf data

    Store the configuration of the program
-}
data Conf = Conf {
    iFile :: Maybe String,
    oFormat :: Maybe ConfFormat,
    oFile :: Maybe String,
    iFormat :: Maybe ConfFormat,
    calcMode :: Maybe Bool,
    csvMode :: Maybe Bool
} deriving (Show)

{-  | VerifiedConf data

    Store the verified configuration of the program
-}
data VerifiedConf = VerifiedConf {
    _iFile :: String,
    _oFormat :: ConfFormat,
    _oFile :: String,
    _iFormat :: ConfFormat,
    _calcMode :: Bool,
    _csvMode :: Bool
} deriving (Show)

-- Private functions

{-  | myError function

    Print an error message and exit the program with a failure code
-}
myError :: String -> IO ()
myError str = hPutStrLn stderr str >>
    hPutStr stderr "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-c]" >>
    hPutStrLn stderr "[-e iformat]" >>
    hPutStrLn stderr "\tifile\t\tpath to the file to convert" >>
    hPutStrLn stderr "\toformat\t\toutput format (xml, json, markdown)" >>
    hPutStrLn stderr "\tofile\t\tpath to the output file" >>
    hPutStrLn stderr "\tiformat\t\tinput format (xml, json, markdown)" >>
    hPutStrLn stderr "\t-c\t\tto enter calculator mode" >>
    hPutStrLn stderr "\t-cvs\t\tto enter CSV parser mode" >>
    exitWith (ExitFailure 84)

-- Public functions

{-  | defaultConf value

    Default configuration of the program
-}
defaultConf :: Conf
defaultConf = Conf {
    iFile = Nothing,
    oFormat = Nothing,
    oFile = Nothing,
    iFormat = Just UNKNOWN,
    calcMode = Just False,
    csvMode = Just False
}

{- | getFormat function

    Get the format from a string

    Return Just the format if it is valid, Nothing otherwise
-}
getFormat :: String -> Maybe ConfFormat
getFormat "json" = Just JSON
getFormat "xml" = Just XML
getFormat "markdown" = Just MARKDOWN
getFormat _ = Nothing

{-  | getOpts function

    Get the options from the command line

    Return Just the configuration if it is valid, Nothing otherwise
-}
getOpts :: Conf -> [String] -> Maybe Conf
getOpts conf [] = Just conf
getOpts conf ("-i":"\"\"":xs) = getOpts conf{iFile = Just "\0"} xs
getOpts conf ("-i":x:xs) = getOpts conf{iFile = Just x} xs
getOpts conf ("-f":x:xs) = case getFormat x of
    Nothing -> Nothing
    Just format -> getOpts conf{oFormat = Just format} xs
getOpts conf ("-o":"":xs) = getOpts conf{oFile = Just "\0"} xs
getOpts conf ("-o":x:xs) = getOpts conf{oFile = Just x} xs
getOpts conf ("-e":x:xs) = case getFormat x of
    Nothing -> Nothing
    Just format -> getOpts conf{iFormat = Just format} xs
getOpts conf ("-c":xs) = getOpts conf{calcMode = Just True} xs
getOpts conf ("-csv":xs) = getOpts conf{csvMode = Just True} xs
getOpts _ _ = Nothing

{-  | validateConf function

    Validate the configuration

    Return Nothing if it is valid, Just the error message otherwise
-}
validateConf :: Maybe Conf -> IO ()
validateConf Nothing = myError "Error:\n\tinvalid arguments.\n"
validateConf (Just (Conf Nothing _ _ _ _ _)) =
    myError "Error:\n\ti is missing."
validateConf (Just (Conf _ Nothing _ _ _ _)) =
    myError "Error:\n\tf is missing."
validateConf _ = return ()

{-  | createVerifiedConf function

    Return the verified configuration
-}
createVerifiedConf :: Conf -> VerifiedConf
createVerifiedConf conf = VerifiedConf {
    _iFile = fromMaybe "" (iFile conf),
    _oFormat = fromMaybe UNKNOWN (oFormat conf),
    _oFile = fromMaybe "" (oFile conf),
    _iFormat = fromMaybe UNKNOWN (iFormat conf),
    _calcMode = fromMaybe False (calcMode conf),
    _csvMode = fromMaybe False (csvMode conf)
}
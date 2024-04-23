{-
-- EPITECH PROJECT, 2024
-- Pandoc
-- File description:
-- Config
-}

module Config(
    Conf(..),
    VerifiedConf(..),
    Format(..),
    defaultConf,
    getOpts,
    validateConf,
    myError,
    createVerifiedConf
) where

import Data.Maybe(fromMaybe)
import System.Exit(exitWith, ExitCode(ExitFailure))
import System.IO (hPutStrLn, hPutStr, stderr)

data Format = JSON | XML | MARKDOWN | UNKNOWNED deriving (Enum, Show, Eq)

{-  | Conf data

    Store the configuration of the program
-}
data Conf = Conf {
    iFile :: Maybe String,
    oFormat :: Maybe Format,
    oFile :: Maybe String,
    iFormat :: Maybe Format
} deriving (Show)

instance Eq Conf where
    (==) (Conf Nothing _ _ _) (Conf (Just _) _ _ _) = False
    (==) (Conf (Just _) _ _ _) (Conf Nothing _ _ _) = False

    (==) (Conf _ Nothing _ _) (Conf _ (Just _) _ _) = False
    (==) (Conf _ (Just _) _ _) (Conf _ Nothing _ _) = False

    (==) (Conf _ _ Nothing _) (Conf _ _ (Just _) _) = False
    (==) (Conf _ _ (Just _) _) (Conf _ _ Nothing _) = False

    (==) (Conf _ _ _ Nothing) (Conf _ _ _ (Just _)) = False
    (==) (Conf _ _ _ (Just _)) (Conf _ _ _ Nothing) = False

    (==) (Conf _ _ _ _) (Conf _ _ _ _) = True -- remove warnings.

{-  | VerifiedConf data

    Store the verified configuration of the program
-}
data VerifiedConf = VerifiedConf {
    _iFile :: String,
    _oFormat :: Format,
    _oFile :: String,
    _iFormat :: Format
} deriving (Show)

-- Private functions

{-  | myError function

    Print an error message and exit the program with a failure code
-}
myError :: String -> IO ()
myError str =
    hPutStrLn stderr str >>
    hPutStr stderr "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] " >>
    hPutStrLn stderr "[-e iformat]" >>
    hPutStrLn stderr "\tifile\t\tpath to the file to convert" >>
    hPutStrLn stderr "\toformat\t\toutput format (xml, json, markdown)" >>
    hPutStrLn stderr "\tofile\t\tpath to the output file" >>
    hPutStrLn stderr "\tiformat\t\tinput format (xml, json, markdown)" >>
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
    iFormat = Just UNKNOWNED
}

{- | getFormat function

    Get the format from a string

    Return Just the format if it is valid, Nothing otherwise
-}
getFormat :: String -> Maybe Format
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
getOpts conf ("-i":x:xs) = getOpts conf{iFile = Just x} xs
getOpts conf ("-f": x:xs) = case getFormat x of
    Nothing -> Nothing
    Just format -> getOpts conf{oFormat = Just format} xs
getOpts conf ("-o": x:xs) = getOpts conf{oFile = Just x} xs
getOpts conf ("-e": x:xs) = case getFormat x of
    Nothing -> Nothing
    Just format -> getOpts conf{iFormat = Just format} xs
getOpts _ _ = Nothing

{-  | validateConf function

    Validate the configuration

    Return Nothing if it is valid, Just the error message otherwise
-}
validateConf :: Maybe Conf -> IO ()
validateConf Nothing = myError "Error:\n\tinvalid arguments.\n"
validateConf (Just (Conf Nothing _ _ _)) =
    myError "Error:\n\ti is missing."
validateConf (Just (Conf _ Nothing _ _)) =
    myError "Error:\n\tf is missing."
validateConf _ = return ()

{-  | createVerifiedConf function

    Return the verified configuration
-}
createVerifiedConf :: Conf -> VerifiedConf
createVerifiedConf conf = VerifiedConf {
    _iFile = fromMaybe "" (iFile conf),
    _oFormat = fromMaybe UNKNOWNED (oFormat conf),
    _oFile = fromMaybe "" (oFile conf),
    _iFormat = fromMaybe UNKNOWNED (iFormat conf)
}
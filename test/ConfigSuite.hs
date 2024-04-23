{-
-- EPITECH PROJECT, 2024
-- Pandoc
-- File description:
-- ConfigSuite
-}

module ConfigSuite (
    configSuite
) where

import Test.HUnit
import Config (
    Format(..),
    Conf(..),
    defaultConf,
    getOpts)

getOptsEmptyDefault :: Test
getOptsEmptyDefault = TestCase $ assertEqual "getOpts []"
    (Just conf) (getOpts conf [])
    where conf = defaultConf

getOptsEmptyCustom :: Test
getOptsEmptyCustom = TestCase $ assertEqual "getOpts []"
    (Just conf) (getOpts conf [])
    where conf = (Conf (Just "aaa") (Just XML) (Just "bbb") (Just JSON))

getOptsIFile :: Test
getOptsIFile = TestCase $ assertEqual "getOpts -i file"
    (Just conf) (getOpts defaultConf ["-i", "file"])
    where conf = (Conf (Just "file") Nothing Nothing (Just UNKNOWNED))

getOptsIFileEmpty :: Test
getOptsIFileEmpty = TestCase $ assertEqual "getOpts -i"
    Nothing (getOpts defaultConf ["-i"])

getOptsOFile :: Test
getOptsOFile = TestCase $ assertEqual "getOpts -o file"
    (Just conf) (getOpts defaultConf ["-o", "file"])
    where conf = (Conf Nothing Nothing (Just "file") (Just UNKNOWNED))

getOptsOFileEmpty :: Test
getOptsOFileEmpty = TestCase $ assertEqual "getOpts -o"
    Nothing (getOpts defaultConf ["-o"])

getOptsIFormat :: Test
getOptsIFormat = TestCase $ assertEqual "getOpts -e xml"
    (Just conf) (getOpts defaultConf ["-e", "xml"])
    where conf = (Conf Nothing Nothing Nothing (Just XML))

getOptsIFormatEmpty :: Test
getOptsIFormatEmpty = TestCase $ assertEqual "getOpts -e"
    Nothing (getOpts defaultConf ["-e"])

getOptsIFormatBad :: Test
getOptsIFormatBad = TestCase $ assertEqual "getOpts -f house"
    Nothing (getOpts defaultConf ["-f", "house"])

getOptsOFormat :: Test
getOptsOFormat = TestCase $ assertEqual "getOpts -f json"
    (Just conf) (getOpts defaultConf ["-f", "json"])
    where conf = (Conf Nothing (Just JSON) Nothing (Just UNKNOWNED))

getOptsOFormatEmpty :: Test
getOptsOFormatEmpty = TestCase $ assertEqual "getOpts -f"
    Nothing (getOpts defaultConf ["-f"])

getOptsOFormatBad :: Test
getOptsOFormatBad = TestCase $ assertEqual "getOpts -f car"
    Nothing (getOpts defaultConf ["-f", "car"])

getOptsSuite :: Test
getOptsSuite = TestList [
        TestLabel "getOpts empty + default" getOptsEmptyDefault,
        TestLabel "getOpts empty + custom" getOptsEmptyCustom,

        TestLabel "getOpts iFile + default" getOptsIFile,
        TestLabel "getOpts iFile empty" getOptsIFileEmpty,
        TestLabel "getOpts oFile + default" getOptsOFile,
        TestLabel "getOpts oFile empty" getOptsOFileEmpty,

        TestLabel "getOpts iFormat + default" getOptsIFormat,
        TestLabel "getOpts iFormat empty" getOptsIFormatEmpty,
        TestLabel "getOpts iFormat bad" getOptsIFormatBad,
        TestLabel "getOpts oFormat + default" getOptsOFormat,
        TestLabel "getOpts oFormat empty" getOptsOFormatEmpty,
        TestLabel "getOpts oFormat bad" getOptsOFormatBad
        ]

configSuite :: Test
configSuite = do
    TestList [getOptsSuite]

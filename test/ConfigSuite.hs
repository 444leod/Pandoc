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
import Config

getOptsEmptyDefault :: Test
getOptsEmptyDefault = TestCase $ assertEqual "getOpts []"
    (Just conf) (getOpts conf [])
    where conf = defaultConf

getOptsEmptyCustom :: Test
getOptsEmptyCustom = TestCase $ assertEqual "getOpts []"
    (Just conf) (getOpts conf [])
    where conf = Conf (Just "aaa") (Just XML) (Just "bbb") (Just JSON)

getOptsIFile :: Test
getOptsIFile = TestCase $ assertEqual "getOpts -i file"
    (Just conf) (getOpts defaultConf ["-i", "file"])
    where conf = Conf (Just "file") Nothing Nothing (Just UNKNOWN)

getOptsIFileEmpty :: Test
getOptsIFileEmpty = TestCase $ assertEqual "getOpts -i"
    Nothing (getOpts defaultConf ["-i"])

getOptsOFile :: Test
getOptsOFile = TestCase $ assertEqual "getOpts -o file"
    (Just conf) (getOpts defaultConf ["-o", "file"])
    where conf = Conf Nothing Nothing (Just "file") (Just UNKNOWN)

getOptsOFileEmpty :: Test
getOptsOFileEmpty = TestCase $ assertEqual "getOpts -o"
    Nothing (getOpts defaultConf ["-o"])

getOptsIFormat :: Test
getOptsIFormat = TestCase $ assertEqual "getOpts -e xml"
    (Just conf) (getOpts defaultConf ["-e", "xml"])
    where conf = Conf Nothing Nothing Nothing (Just XML)

getOptsIFormatEmpty :: Test
getOptsIFormatEmpty = TestCase $ assertEqual "getOpts -e"
    Nothing (getOpts defaultConf ["-e"])

getOptsIFormatBad :: Test
getOptsIFormatBad = TestCase $ assertEqual "getOpts -f house"
    Nothing (getOpts defaultConf ["-e", "house"])

getOptsOFormat :: Test
getOptsOFormat = TestCase $ assertEqual "getOpts -f json"
    (Just conf) (getOpts defaultConf ["-f", "json"])
    where conf = Conf Nothing (Just JSON) Nothing (Just UNKNOWN)

getOptsOFormatEmpty :: Test
getOptsOFormatEmpty = TestCase $ assertEqual "getOpts -f"
    Nothing (getOpts defaultConf ["-f"])

getOptsOFormatBad :: Test
getOptsOFormatBad = TestCase $ assertEqual "getOpts -f car"
    Nothing (getOpts defaultConf ["-f", "car"])

getOptsMany1 :: Test
getOptsMany1 = TestCase $ assertEqual "getOpts -i in -f md -o out"
    (Just c) (getOpts defaultConf ["-i", "in", "-o", "out", "-f", "markdown"])
    where c = Conf (Just "in") (Just MARKDOWN) (Just "out") (Just UNKNOWN)

getOptsMany2 :: Test
getOptsMany2 = TestCase $ assertEqual "getOpts -i in -i huh -i car"
    conf (getOpts defaultConf ["-i", "in", "-i", "huh", "-i", "car"])
    where conf = Just (Conf (Just "car") Nothing Nothing (Just UNKNOWN))

getOptsMany3 :: Test
getOptsMany3 = TestCase $ assertEqual "getOpts -i -e -f -o -i"
    Nothing (getOpts defaultConf ["-i", "-e", "-f", "-o", "-i"])

getOptsMany4 :: Test
getOptsMany4 = TestCase $ assertEqual "getOpts -f car -i car"
    Nothing (getOpts defaultConf ["-i", "-e", "-f", "-o", "-i"])

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
        TestLabel "getOpts oFormat bad" getOptsOFormatBad,

        TestLabel "gotOpts many args I" getOptsMany1,
        TestLabel "gotOpts many args II" getOptsMany2,
        TestLabel "gotOpts many args III" getOptsMany3,
        TestLabel "gotOpts many args IV" getOptsMany4
        ]

configSuite :: Test
configSuite = do
    TestList [getOptsSuite]

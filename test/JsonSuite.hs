--
-- EPITECH PROJECT, 2024
-- Pandoc
-- File description:
-- JsonSuite
--

module JsonSuite (jsonSuite) where

import Json
import Test.HUnit
import ParserLib (Parser(runParser))

parseJsonValueNull :: Test
parseJsonValueNull = TestCase $ assertEqual "parse null"
        (Just (Null, "")) (runParser parseJsonValue "null")

parseJsonValueTrue :: Test
parseJsonValueTrue = TestCase $ assertEqual "parse true"
        (Just (Boolean True, "")) (runParser parseJsonValue "true")

parseJsonValueFalse :: Test
parseJsonValueFalse = TestCase $ assertEqual "parse false"
        (Just (Boolean False, "")) (runParser parseJsonValue "false")

parseJsonNumberPos :: Test
parseJsonNumberPos = TestCase $ assertEqual "parse 12"
        (Just (Number 12, "")) (runParser parseJsonValue "12")

parseJsonNumberNeg :: Test
parseJsonNumberNeg = TestCase $ assertEqual "parse 12"
        (Just (Number (-12), "")) (runParser parseJsonValue "-12")

parseJsonArrayNums :: Test
parseJsonArrayNums = TestCase $ assertEqual "parse [1, 2, 3]"
        (Just (JArray [Number 1, Number 2, Number 3], ""))
        (runParser parseJsonValue "[1, 2, 3]")

parseJsonArrayStrings :: Test
parseJsonArrayStrings = TestCase $ assertEqual "parse [A, BC, DEF]"
        (Just (JArray [JString "A", JString "BC", JString "DEF"], ""))
        (runParser parseJsonValue "[\"A\", \"BC\", \"DEF\"]")

parsePrimitiveSuite :: Test
parsePrimitiveSuite = TestList [
        TestLabel "Parse Null" parseJsonValueNull,
        TestLabel "Parse True" parseJsonValueTrue,
        TestLabel "Parse False" parseJsonValueFalse,
        TestLabel "Parse 12" parseJsonNumberPos,
        TestLabel "Parse -12" parseJsonNumberNeg,
        TestLabel "Parse [1 2 3]" parseJsonArrayNums,
        TestLabel "Parse [A BC DEF]" parseJsonArrayStrings
        ]

jsonSuite :: Test
jsonSuite = do
    TestList [parsePrimitiveSuite]

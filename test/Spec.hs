{-
-- EPITECH PROJECT, 2024
-- FUNCTIONNAL
-- File description:
-- Spec
-}

import Test.HUnit
import Text.Printf

main :: IO ()
main = do
    tests <- testList
    (c, _) <- runTestText (putErrors $ testCaseCount tests) tests
    putCounts c

putErrors :: Int -> PutText Int
putErrors 0 = PutText put 0
    where put _ _ _ = return 0
putErrors count = PutText put (count)
    where   put _ _ (-1) = return 0
            put _ False val = return (val - 1)
            put str _ val =
                putStr "\ESC[31m" >> putStrLn str >> putStr "\ESC[0m" >>
                return (val - 1)

putCounts :: Counts -> IO ()
putCounts (Counts _ tries fails errs) =
    putStrLn "\nEnd of tests. Result are:\n" >>
    printf "\ESC[32m%d successes\n" success >>
    printf "\ESC[33m%d fails\n" fails >>
    printf "\ESC[31m%d errors\n" errs >>
    printf "\ESC[0mOut of %d tests\n" tries
    where success = tries - (fails + errs)

testList :: IO Test
testList = do
    return $ TestList [
        ]

module Main (main) where

import System.Exit
import Test.HUnit
import qualified Data.Map as Map

import Lib
import Eval0
import Eval1
import Eval2
import Eval3
import Eval4
import Eval5
import Eval6


testEval0 :: Test
testEval0 = TestCase (assertEqual "testEval0"
                        (IntVal 18)
                        (eval0 Map.empty exampleExp)) --exampleExp is defined in src/Lib.hs

testEval1 :: Test
testEval1 = TestCase (assertEqual "testEval1"
                        (IntVal 18)
                        (runEval1 (eval1 Map.empty exampleExp)))

testEval2a :: Test
testEval2a = TestCase (assertEqual "testEval2a"
                                  (Right (IntVal 18))
                                  (runEval2(eval2a Map.empty exampleExp)))

--testEval2aError :: Test
--testEval2aError = TestCase (assertEqual
--                            "testEval2aError"
--                            (Left "type error")
--                            runEval2(eval2a Map.empty (Plus(Lit 1) (Abs "x" (Var "x"))))))

--testEval2aKeyNotFound :: Test
--testEval2aKeyNotFound = TestCase (assertEqual "testEval2aKeyNotFound"
--                                 (Left "undefined variable: x")
--                                 (runEval2(eval2b Map.empty(Var "x"))))


testEval3 :: Test
testEval3 = TestCase (assertEqual "testEval3"
                     (Right (IntVal 18))
                     (runEval3 Map.empty (eval3 exampleExp)))


testEval4 :: Test
testEval4 = TestCase (assertEqual "testEval4"
                     (Right (IntVal 18),8)
                     (runEval4 Map.empty 0 (eval4 exampleExp)))

testEval5 :: Test
testEval5 = TestCase (assertEqual "testEval5"
                     ((Right (IntVal 18),["x"]),8)
                     (runEval5 Map.empty 0 (eval5 exampleExp)))


testEval6 :: Test
testEval6 = TestCase (do putStrLn "\nTest 6 output:"
                         actual <- runEval6 Map.empty 0 (eval6 exampleExp)
                         let expected = ((Right (IntVal 18),["x"]),8)
                         putStrLn "Test 6 output end.\n"
                         assertEqual "testEval6" expected actual)

main :: IO ()
main = do
  allCounts <- runTestTT (test [testEval0,
                                testEval1,
                                testEval2a,
--                              testEval2aError,
--                              testEval2aKeyNotFound
                                testEval3,
                                testEval4,
                                testEval5,
                                testEval6
                                ])
  if errors allCounts + failures allCounts == 0
    then exitSuccess
    else exitFailure


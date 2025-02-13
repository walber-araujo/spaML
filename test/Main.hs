module Main (main) where
import Test.HUnit
import ClassifierTest (classifierTests)
import ModelTest (modelTests)
tests :: Test
tests = TestList [classifierTests, modelTests]

main :: IO Counts
main = runTestTT tests

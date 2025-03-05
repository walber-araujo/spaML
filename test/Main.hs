module Main (main) where
import Test.HUnit
import ClassifierTest (classifierTests)
import ModelTest (modelTests)
import PreprocessingTest (preprocessingTests)
import TrainingTest (trainingTests)

tests :: Test
tests = TestList [classifierTests, modelTests, preprocessingTests, trainingTests]

main :: IO Counts
main = runTestTT tests

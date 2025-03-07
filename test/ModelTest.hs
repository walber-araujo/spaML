-- Testes para a estrutura do modelo

module ModelTest where

import Model ( calculateWordProbabilities, countWords )
import Test.HUnit
import qualified Data.Vector as V
import qualified Data.Map as Map
import Control.Exception (try, evaluate)
import qualified Control.Exception as E
import Utils

modelTests = TestList [
  TestLabel "countWords_empty" testCountWordsEmpty,
  TestLabel "countWords_singleHam" testCountWordsSingleHam,
  TestLabel "countWords_singleSpam" testCountWordsSingleSpam,
  TestLabel "countWords_multiple" testCountWordsMultiple,
  TestLabel "calculate_probabilities" testCalculateProbabilities,
  TestLabel "probabilities_divisionByZero" testProbabilitiesDivisionByZero
  ]

createRecords :: [(String, String)] -> V.Vector MyRecord
createRecords = V.fromList . map (uncurry MyRecord)

testCountWordsEmpty :: Test
testCountWordsEmpty = TestCase $
  let records = V.empty
      (hamW, spamW, hamC, spamC) = countWords records Map.empty Map.empty (0, 0)
  in do assertEqual "Empty ham words" Map.empty hamW
        assertEqual "Empty spam words" Map.empty spamW
        assertEqual "Zero ham count" 0 hamC
        assertEqual "Zero spam count" 0 spamC

testCountWordsSingleHam :: Test
testCountWordsSingleHam = TestCase $
  let records = createRecords [("ham", "hello world")]
      (hamW, spamW, hamC, spamC) = countWords records Map.empty Map.empty (0, 0)
  in do assertEqual "Ham words count" (Map.fromList [("hello",1), ("world",1)]) hamW
        assertEqual "Ham count" 1 hamC
        assertEqual "Empty spam words" Map.empty spamW
        assertEqual "Zero spam count" 0 spamC

testCountWordsSingleSpam :: Test
testCountWordsSingleSpam = TestCase $
  let records = createRecords [("spam", "buy now")]
      (hamW, spamW, hamC, spamC) = countWords records Map.empty Map.empty (0, 0)
  in do assertEqual "Empty ham words" Map.empty hamW
        assertEqual "Spam words count" (Map.fromList [("buy",1), ("now",1)]) spamW
        assertEqual "Zero ham count" 0 hamC
        assertEqual "Spam count" 1 spamC

testCountWordsMultiple :: Test
testCountWordsMultiple = TestCase $
  let records = createRecords [
        ("ham", "hello"),
        ("ham", "hello again"),
        ("spam", "buy now"),
        ("spam", "hello buy")
        ]
      (hamW, spamW, hamC, spamC) = countWords records Map.empty Map.empty (0, 0)
  in do assertEqual "Ham words" (Map.fromList [("hello",2), ("again",1)]) hamW
        assertEqual "Spam words" (Map.fromList [("buy",2), ("now",1), ("hello",1)]) spamW
        assertEqual "Ham count" 2 hamC
        assertEqual "Spam count" 2 spamC

testCalculateProbabilities :: Test
testCalculateProbabilities = TestCase $
  -- pWordGivenHam = (4+1)/(2+1) = 5/3
  -- pWordGivenSpam = (1+1)/(1+1) = 1
  -- ratio = 5/3 ≈ 1.666...
  let hamWords = Map.fromList [("test", 4)]
      hamCount = 2
      spamWords = Map.fromList [("test", 1)]
      spamCount = 1
      result = calculateWordProbabilities hamWords hamCount spamWords spamCount Map.empty
  in assertEqual "Test probability calculation"
     (Map.fromList [("test", 5/3)])
     result

testProbabilitiesDivisionByZero :: Test
testProbabilitiesDivisionByZero = TestCase $
  let hamWords = Map.fromList [("test", 4)]
      hamCount = 2
      spamWords = Map.empty
      spamCount = 0
      result = calculateWordProbabilities hamWords hamCount spamWords spamCount Map.empty
  in assertEqual "Test probability division by zero"
    (Map.fromList [("test", 0)])
    result

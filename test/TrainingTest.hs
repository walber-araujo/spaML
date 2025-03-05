-- Testes unitários para o processo de treinamento

module TrainingTest where

import Test.HUnit
import qualified Data.Vector as V
import qualified Data.Map as Map
import Training
import Model
import Utils

createRecords :: [(String, String)] -> V.Vector MyRecord
createRecords = V.fromList . map (uncurry MyRecord)

sampleRecords = createRecords [
    ("ham", "hello"),
    ("ham", "hello again"),
    ("spam", "buy now"),
    ("spam", "hello buy")
    ]

-- Teste para verificar se o modelo treina corretamente
testTrainModel :: Test
testTrainModel = TestCase $ 
    let (hamProbs, spamProbs, hamCount, spamCount) = trainModel sampleRecords
    in do
        assertEqual "Test ham probabilities"
            (Map.fromList [("hello", (3/4)/(2/5)), ("again", (2/4)/(1/5))])
            hamProbs
        assertEqual "Test spam probabilities"
            (Map.fromList [("hello", (2/5)/(3/4)), ("buy", (3/5)/(1/4)), ("now", (2/5)/(1/4))])
            spamProbs
        assertEqual "Ham count should be 2" 2 hamCount
        assertEqual "Spam count should be 2" 2 spamCount

trainingTests :: Test
trainingTests = TestList [testTrainModel]

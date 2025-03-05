-- Testes unitários para a lógica de classificação

module ClassifierTest where

import Test.HUnit
import qualified Data.Map as Map
import Classifier

-- Dados de exemplo para os testes
hamProbs :: Map.Map String Double
hamProbs = Map.fromList [("hello", 0.8), ("world", 0.2)]

spamProbs :: Map.Map String Double
spamProbs = Map.fromList [("buy", 0.9), ("now", 0.7)]

testHam :: Test
testHam = TestCase $ 
    assertEqual "Message 'hello world' should be classified as ham" 
                 0 
                 (classifyMessage hamProbs spamProbs "hello world")

testSpam :: Test
testSpam = TestCase $ 
    assertEqual "Message 'buy now' should be classified as spam" 
                 1 
                 (classifyMessage hamProbs spamProbs "buy now")

testNeutral :: Test
testNeutral = TestCase $ 
    assertEqual "Message 'hello buy' should be classified as spam (due to higher spam probability)" 
                 1 
                 (classifyMessage hamProbs spamProbs "hello buy")

classifierTests :: Test
classifierTests = TestList [testHam, testSpam, testNeutral]

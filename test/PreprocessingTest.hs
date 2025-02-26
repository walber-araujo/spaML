-- Testes para o pré-processamento de dados

module PreprocessingTest where

import Test.HUnit
import Preprocessing

-- Teste para tokenização simples
testTokenizeSimple :: Test
testTokenizeSimple = TestCase $ 
    assertEqual "Tokenizing 'Hello world'" 
                 ["Hello", "world"]
                 (tokenize "Hello world")

-- Teste para remoção de pontuação
testTokenizePunctuation :: Test
testTokenizePunctuation = TestCase $ 
    assertEqual "Tokenizing 'Hello, world!'" 
                 ["Hello", "world"]
                 (tokenize "Hello, world!")

-- Teste para múltiplos espaços
testTokenizeMultipleSpaces :: Test
testTokenizeMultipleSpaces = TestCase $ 
    assertEqual "Tokenizing '  Hello   world  '" 
                 ["Hello", "world"]
                 (tokenize "  Hello   world  ")

preprocessingTests :: Test
preprocessingTests = TestList [testTokenizeSimple, testTokenizePunctuation, testTokenizeMultipleSpaces]

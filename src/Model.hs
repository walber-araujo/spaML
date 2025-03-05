module Model where

{-
Module      : Model
Description : Model data structure (probabilities, word count).
Stability   : stable.
-}

import qualified Data.Vector as V
import qualified Data.Map as Map
import Data.List (foldl')
import Preprocessing
import Utils

-- Função para contar as palavras por classe (ham ou spam)
countWords :: V.Vector MyRecord -> Map.Map String Int -> Map.Map String Int -> (Int, Int) -> (Map.Map String Int, Map.Map String Int, Int, Int)
countWords records hamWords spamWords (hamCount, spamCount) = 
    V.foldl' updateCounts (hamWords, spamWords, hamCount, spamCount) records
  where
    updateCounts (hWords, sWords, hCount, sCount) record =
      let tokens = tokenize (message record)
          lbl = label record
          (newHWords, newSWords, newHCount, newSCount) =
            if lbl == "ham" then
                (foldl' (\acc word -> Map.insertWith (+) word 1 acc) hWords tokens, sWords, hCount + 1, sCount)
            else
                (hWords, foldl' (\acc word -> Map.insertWith (+) word 1 acc) sWords tokens, hCount, sCount + 1)
      in (newHWords, newSWords, newHCount, newSCount)

-- Função para calcular a probabilidade de uma palavra para cada classe
calculateWordProbabilities :: Map.Map String Int -> Int -> Map.Map String Int -> Int -> Map.Map String Double -> Map.Map String Double
calculateWordProbabilities wordsCount totalCount otherWordsCount otherCount wordProbs =
    Map.foldrWithKey updateProb wordProbs wordsCount
  where
    updateProb word count acc =
        let pWordGivenClass = (fromIntegral count + 1) / (fromIntegral totalCount + fromIntegral (Map.size wordsCount)) -- Laplace smoothing
            otherCountWord = Map.findWithDefault 0 word otherWordsCount
            pOtherClassGivenWord = (fromIntegral otherCountWord + 1) / (fromIntegral otherCount + fromIntegral (Map.size otherWordsCount)) -- Laplace smoothing
        in Map.insert word (pWordGivenClass / pOtherClassGivenWord) acc
module Training where

-- Lógica de treinamento do modelo

import qualified Data.Vector as V
import Model

-- Função para treinar o modelo
trainModel :: V.Vector MyRecord -> (Map.Map String Double, Map.Map String Double, Int, Int)
trainModel records = 
    let (hamWords, spamWords, hamCount, spamCount) = countWords records Map.empty Map.empty (0, 0)
        hamProbs = calculateWordProbabilities hamWords hamCount spamWords spamCount Map.empty
        spamProbs = calculateWordProbabilities spamWords spamCount hamWords hamCount Map.empty
    in (hamProbs, spamProbs, hamCount, spamCount)

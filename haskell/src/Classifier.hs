module Classifier where

-- Lógica de classificação

import qualified Data.Map as Map
import Preprocessing

-- Função para classificar uma nova mensagem
classifyMessage :: Map.Map String Double -> Map.Map String Double -> String -> Double
classifyMessage hamProbs spamProbs msg =
    let tokens = tokenize msg
        hamProb = sum [Map.findWithDefault 0 word hamProbs | word <- tokens]
        spamProb = sum [Map.findWithDefault 0 word spamProbs | word <- tokens]
    in if hamProb > spamProb then 0 else 1 -- 0 = ham, 1 = spam
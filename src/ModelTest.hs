module ModelTest where

-- Função para testar o modelo e calcular a acurácia

import qualified Data.Vector as V
import qualified Data.Map as Map
import Data.List (foldl')
import Classifier
import Utils

-- Testa o modelo classificando cada mensagem e calcula a precisão.  
-- Retorna a fração de classificações corretas sobre o total de registros. 
testModel :: V.Vector MyRecord -> Map.Map String Double -> Map.Map String Double -> IO Double
testModel records hamProbs spamProbs = do
    let correct = foldl' (\acc record -> 
                            let result = classifyMessage hamProbs spamProbs (message record)
                            in if (result == 0 && label record == "ham") || (result == 1 && label record == "spam") then acc + 1 else acc) 
                         0 records
        total = V.length records
    return (fromIntegral correct / fromIntegral total)
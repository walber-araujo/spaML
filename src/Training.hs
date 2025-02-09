module Training where

-- Lógica de treinamento do modelo

import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Text.Printf (printf)
import Model
import ModelTest
import Utils

-- Função para treinar o modelo
trainModel :: V.Vector MyRecord -> (Map.Map String Double, Map.Map String Double, Int, Int)
trainModel records = 
    let (hamWords, spamWords, hamCount, spamCount) = countWords records Map.empty Map.empty (0, 0)
        hamProbs = calculateWordProbabilities hamWords hamCount spamWords spamCount Map.empty
        spamProbs = calculateWordProbabilities spamWords spamCount hamWords hamCount Map.empty
    in (hamProbs, spamProbs, hamCount, spamCount)

trainModelCSV :: FilePath -> IO (Map.Map String Double, Map.Map String Double)
trainModelCSV filePath = do
    -- Carrega o arquivo CSV
    arquivoCSV <- BL.readFile filePath

    clearTerminal
    putStrLn ("CSV file loaded from " ++ filePath)

    -- Fazer o parsing do CSV
    let registros = decode HasHeader arquivoCSV :: Either String (V.Vector MyRecord)
    
    -- Verificar o resultado e exibir
    case registros of
        Left err -> do
            putStrLn $ "Error reading the CSV: " ++ err
            return (Map.empty, Map.empty) -- Retorno padrão em caso de erro

        Right rgs -> do
            -- Dividir o dataset em treino e teste
            let (trainSet, testSet) = divideDataset rgs

            -- Treinar o modelo
            let (hamProbs, spamProbs, _, _) = trainModel trainSet

            -- Calcular a acurácia no conjunto de teste
            accuracy <- testModel testSet hamProbs spamProbs
            putStrLn $ "Model accuracy on the test set: " ++ printf "%.2f" (accuracy * 100) ++ "%\n"

            return (hamProbs, spamProbs)
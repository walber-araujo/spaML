module Metric where

-- Treina o modelo para apresentar sua acurácia

import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension, takeFileName, dropExtension)
import Text.Printf (printf)
import System.IO
import Data.Csv
import ModelTest
import Utils
import Training

-- Calcula a acurácia do modelo passado
showAccuracy :: FilePath -> IO Double
showAccuracy filePath = do

    fileCsv <- BL.readFile filePath

    -- Decodifica os arquivos CSV
    let registros = decode HasHeader fileCsv :: Either String (V.Vector MyRecord)

    case registros of
        Left err -> do
            putStrLn $ "Error reading the CSV: " ++ err
            return 0.0 -- Retorno padrão em caso de erro

        Right rgs -> do
            -- Dividir o dataset em treino e teste
            
            (trainSet, testSet) <- divideCsvTrainingTest filePath rgs

            -- Treinar o modelo
            let (hamProbs, spamProbs, _, _) = trainModel trainSet

            -- Calcular a acurácia no conjunto de teste
            accuracy <- testModel testSet hamProbs spamProbs         

            return (fromIntegral (truncate (accuracy * 10000)) / 100)

-- Encontra caminhos dos arquivos CSVs para calcular acurácia
findCSVsPaths :: FilePath -> IO [FilePath]
findCSVsPaths folder = do
                  arquivos <- listDirectory folder
                  return [folder </> arq | arq <- arquivos, takeExtension arq == ".csv"]

--Realiza recursão para mostrar a acurácia de cada modelo em Data
accuracyRecursion :: [FilePath] -> IO()
accuracyRecursion [] = putStrLn ("The default model accuracy is calculated by training the model, " ++
                      "where 30% of the data from the file is used for training, and 70% is reserved for testing. " ++
                      "When is used other model criated by the user are used 100% of the new data to training and 100% of default file to testing. " ++
                      "First, the messages are counted and categorized as spam or ham (not spam). " ++
                      "Then, the model calculates the probability of a message being spam or ham. " ++
                      "Finally, the 30% of the data set aside for testing is processed by the classifier, " ++
                      "which determines whether each message is spam or ham. " ++
                      "The model then evaluates whether the classification was correct or not. " ++
                      "The accuracy is calculated as the ratio of correctly classified messages to the total number of test messages.\n" ++
                      "\nRating ranges: \n" ++
                      "0% - 65% = Bad\n" ++
                      "65% - 85% = Moderate\n" ++
                      "85% - 100% = Good\n" ++
                      "------------------------------------------------------------------------------\n" ++
                      "| File Name                     | Accuracy (%)          | Classification     |\n" ++
                      "------------------------------------------------------------------------------")

accuracyRecursion (h:t) = do
                          accuracyRecursion t
                          accuracy <- showAccuracy h
                          printf "| %-29s | %-20.2f | %-19s |\n" (dropExtension (takeFileName h)) accuracy (modelClassification accuracy)

-- Através do path que contém os modelos de treinamento apresenta a acurácia de cada um
accuracyCSVs :: FilePath -> IO()
accuracyCSVs filePath = do
            
            files <- findCSVsPaths filePath

            accuracyRecursion files

-- Intervalos de classficação do modelo
modelClassification :: Double -> String
modelClassification value
                    | value < 65.0 = "Bad"
                    | value < 85.0 = "Moderate"
                    | otherwise = "Good"
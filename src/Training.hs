module Training where

{-
Module      : Training
Description : Model training logic.
Stability   : stable.
-} 

import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Text.Printf (printf)
import Model
import ModelTest
import Utils

{- |
    Train the model. 
    Parameters:
        - 'V.Vector MyRecord': vector with the words.
    Return:
        - '(Map.Map String Double, Map.Map String Double, Int, Int)': ham probability, 
        spam probability, amount of ham words and amount of spam words. 
-}
trainModel :: V.Vector MyRecord -> (Map.Map String Double, Map.Map String Double, Int, Int)
trainModel records = 
    let (hamWords, spamWords, hamCount, spamCount) = countWords records Map.empty Map.empty (0, 0)
        hamProbs = calculateWordProbabilities hamWords hamCount spamWords spamCount Map.empty
        spamProbs = calculateWordProbabilities spamWords spamCount hamWords hamCount Map.empty
    in (hamProbs, spamProbs, hamCount, spamCount)

{- |
    Calculate the model accuracy and show it with the maps of word 
    probabilities for ham and spam.
    Parameters:
        - 'FilePath': path of file.
    Return:
        - '(Map.Map String Double, Map.Map String Double)': maps of word 
        probabilities for ham and spam.
-}
trainModelCSV :: FilePath -> IO (Map.Map String Double, Map.Map String Double)
trainModelCSV filePath = do
    
    arquivoCSV <- BL.readFile filePath

    clearTerminal
    putStrLn ("CSV file loaded from " ++ filePath)

    let registros = decode HasHeader arquivoCSV :: Either String (V.Vector MyRecord)
    
    case registros of
        Left err -> do
            putStrLn $ "Error reading the CSV: " ++ err
            return (Map.empty, Map.empty)

        Right rgs -> do

            (trainSet, testSet) <- divideCsvTrainingTest filePath rgs

            let (hamProbs, spamProbs, _, _) = trainModel trainSet

            accuracy <- testModel testSet hamProbs spamProbs
            putStrLn $ "Model accuracy on the test set: " ++ printf "%.2f" (accuracy * 100) ++ "%\n"

            return (hamProbs, spamProbs)
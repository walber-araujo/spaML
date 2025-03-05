module Metric where

{-
Module      : Metric
Description : Train the models to show their accuracy.
Stability   : stable.
-}

import qualified Data.Vector as V
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BL
import System.Directory (listDirectory, doesFileExist)
import System.FilePath ((</>), takeExtension, takeFileName, dropExtension)
import Text.Printf (printf)
import System.IO
import Data.Csv
import ModelTest
import Utils
import Training

{- |
    Calculate the model accuracy.
    Parameters:
        - 'FilePath': path of file.
    Return:
        - 'IO Double': value of accuracy.
-}
showAccuracy :: FilePath -> IO Double
showAccuracy filePath = do

    exists <- doesFileExist filePath

    if not exists then return (- 1.0)
    else do
        fileCsv <- BL.readFile filePath

        let registros = decode HasHeader fileCsv :: Either String (V.Vector MyRecord)

        case registros of
            Left err -> do
                putStrLn $ "Error reading the CSV: " ++ err
                return 0.0

            Right rgs -> do
            
                (trainSet, testSet) <- divideCsvTrainingTest filePath rgs

                let (hamProbs, spamProbs, _, _) = trainModel trainSet

                accuracy <- testModel testSet hamProbs spamProbs         

                return (fromIntegral (truncate (accuracy * 10000)) / 100)

{- |
    Performs recursion to show the accuracy of each model.
    Parameters:
        - '[(String, FilePath)]': A vector of map with the key (model name) and value (path of file). 
    Return:
        - 'IO()': a little explanation of the algorithm calculation and a table with name, accuracy and
        classification of each model.
-}
accuracyRecursion :: [(String, FilePath)] -> IO()
accuracyRecursion [] = putStrLn ("The default model accuracy is calculated by training the model, " ++
                      "where 30% of the data from the file is used for training, and 70% is reserved for testing. " ++
                      "When is used other model criated by the user are used 100% of the new data to training and 100% of default file to testing. " ++
                      "First, the messages are counted and categorized as spam or ham (not spam). " ++
                      "Then, the model calculates the probability of a message being spam or ham. " ++
                      "Finally, the data set aside for testing is processed by the classifier, " ++
                      "which determines whether each message is spam or ham. " ++
                      "The model then evaluates whether the classification was correct or not. " ++
                      "The accuracy is calculated as the ratio of correctly classified messages to the total number of test messages.\n" ++
                      "\nRating ranges: \n" ++
                      "0% - 65% = Bad\n" ++
                      "65% - 85% = Moderate\n" ++
                      "85% - 100% = Good\n" ++
                      "------------------------------------------------------------------------------\n" ++
                      "| Model Name                     | Accuracy (%)          | Classification     |\n" ++
                      "------------------------------------------------------------------------------")

accuracyRecursion (h:t) = do
                          accuracyRecursion t
                          accuracy <- showAccuracy (snd h)
                          if accuracy == (-1.0) then printf "| %-30s | %-42s |\n" (fst h) "File path not found"
                          else printf "| %-30s | %-21.2f | %-18s |\n" (fst h) accuracy (modelClassification accuracy)

{- |
    Downloads the list of JSON templates to calculate precision in precisionRecursion.
    Parameters:
        - 'FilePath': Path with the paths and names of the data files. 
    Return:
        - 'IO()': the return of accuracyRecursion.
-}
accuracyCSVs :: FilePath -> IO()
accuracyCSVs filePath = do
            let jsonPath = "./data/models/models.json" 

            modelMap <- loadModelMap jsonPath

            let listaModel = Map.toList modelMap

            accuracyRecursion listaModel

{- |
    Ranges with each accuracy rating.
    Parameters:
        - 'Double': Value of accuracy. 
    Return:
        - 'String': the classification according to accuracy.
-}
modelClassification :: Double -> String
modelClassification value
                    | value < 65.0 = "Bad"
                    | value < 85.0 = "Moderate"
                    | otherwise = "Good"
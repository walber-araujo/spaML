{-# LANGUAGE DeriveGeneric #-}

module Utils where

{-|
Module      : Utils
Description : Utility functions for file handling, data manipulation, and model management.
Stability   : stable
-}

import Data.Csv
import Data.Maybe ( fromMaybe )
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import GHC.Generics
import System.Info (os)
import System.IO (hFlush, stdout, withFile, IOMode(..), appendFile)
import System.Process (callCommand)
import qualified Data.Map as Map
import qualified Data.Aeson as Aeson
import System.Directory (doesFileExist)
import Data.List (isSuffixOf)

{-|
    Represents a record from a CSV file, containing a label and a message.
    
    Fields:
    - `label` : A string representing the type of message (e.g., 'ham' or 'spam').
    - `message` : The actual message content.
-}
data MyRecord = MyRecord
  { label :: String   -- ^ Type of message (ham or spam)
  , message :: String -- ^ Message content
  } deriving (Show, Generic)

instance FromRecord MyRecord

{-|
    Type alias for the model map, which stores models with their associated file paths.

    `ModelMap` is a map that associates model names (strings) with their file paths.
-}
type ModelMap = Map.Map String FilePath

{-|
    Reads a CSV file and decodes its contents into a vector of `MyRecord`.

    Parameters:
      - `filePath` : The path to the CSV file to be read.

    Return:
      - `Either String (V.Vector MyRecord)` : Either an error message if the file can't be decoded, or the vector of records.
-}
readCSV :: FilePath -> IO (Either String (V.Vector MyRecord))
readCSV filePath = do
  csvData <- BL.readFile filePath
  return $ decodeWith defaultDecodeOptions HasHeader csvData

{-|
    Divides a dataset into training and test sets (70% training, 30% test).

    Parameters:
      - `records` : A vector of `MyRecord` containing all the data.

    Return:
      - `(V.Vector MyRecord, V.Vector MyRecord)` : A tuple containing the training and test datasets.
-}
divideDataset :: V.Vector MyRecord -> (V.Vector MyRecord, V.Vector MyRecord)
divideDataset records = 
    let total = V.length records
        trainSize = (total * 3) `div` 10 -- 70% to training
    in V.splitAt trainSize records

{-|
    Divides the dataset into training and test sets based on a predefined CSV file or uses a default dataset.

    Parameters:
      - `filePath` : The path to the CSV file.
      - `records` : A vector of `MyRecord` containing the data to be divided.

    Return:
      - `(V.Vector MyRecord, V.Vector MyRecord)` : The training and test datasets.
-}
divideCsvTrainingTest :: FilePath -> V.Vector MyRecord -> IO (V.Vector MyRecord, V.Vector MyRecord)
divideCsvTrainingTest filePath records = do
                if filePath == "data/train_data/SMSSpamCollection.csv"
                then do
                     return (divideDataset records)
                else do 
                     vectorCsvDefault <- downloadDefault
                     return (records, vectorCsvDefault)

{-|
    Downloads the default dataset and returns it as a vector of `MyRecord`.

    Return:
      - `V.Vector MyRecord` : The default dataset.
-}
downloadDefault :: IO (V.Vector MyRecord)
downloadDefault = do
                    fileCsvDefault <- BL.readFile "data/train_data/SMSSpamCollection.csv"
                    let registrosDefault = decode HasHeader fileCsvDefault :: Either String (V.Vector MyRecord)
                    case registrosDefault of
                        Left err -> return V.empty
                        Right rgsDefault -> return rgsDefault

{-|
    Clears the terminal screen.

    This function clears the terminal based on the operating system.
-}
clearTerminal :: IO ()
clearTerminal = do
    let command = if os == "mingw32" then "cls" else "clear"
    callCommand command

{-|
    Flushes the standard output buffer.

    This function ensures that all pending output is written to the terminal.
-}
flushOutput :: IO ()
flushOutput = hFlush stdout

{-|
    Saves a message along with its classification to a CSV file.

    Parameters:
      - `fileName` : The file to save the data.
      - `classification` : The classification of the message (e.g., 'ham' or 'spam').
      - `message` : The message to be saved.

    This function appends the classification and message to the CSV file and clears the terminal.
-}
saveToCSV :: FilePath -> String -> String -> IO()
saveToCSV fileName classification message = do
  let csvLine = classification ++ "," ++ message ++ "\n"
  appendFile fileName csvLine 

  clearTerminal
  putStrLn "Data saved successfully!\n"

{-|
    Loads a JSON file containing the models and returns them as a `ModelMap`.

    Parameters:
      - `path` : The path to the JSON file containing the models.

    Return:
      - `ModelMap` : A map of model names to file paths.
-}
loadModelMap :: FilePath -> IO ModelMap
loadModelMap path = do
    exists <- doesFileExist path
    if exists
        then do
            jsonData <- BL.readFile path
            case Aeson.decode jsonData of
                Just models -> return models
                Nothing     -> return Map.empty  -- Retorna um mapa vazio se o JSON for inválido
        else return Map.empty

{- |
    Show the availabels models.
    Parameters:
        - 'ModelMap': Map with the models names.
    Return:
        - 'IO ()': Show availabels models
-}
printModels :: ModelMap -> IO ()
printModels models = do
    mapM_ putStrLn (Map.keys models)

{- |
    Update and save the models list.
    Parameters:
        - 'String': Models name.
        - 'FilePath': path of model.
    Return:
        - 'IO ()': message showing that model has been saved.
-}
saveModelToJSON :: String -> FilePath -> IO ()
saveModelToJSON modelName filePath = do
    let jsonPath = "./data/models/models.json"

    existingModels <- loadModelMap jsonPath
    let updatedModels = Map.insert modelName filePath existingModels
    BL.writeFile jsonPath (Aeson.encode updatedModels)
    
    putStrLn "\n✅ Model saved successfully!"

{- |
    Ensure that the csv file has the suffix .csv.
    Parameters:
        - 'String': file name.
    Return:
        - 'String': the file name with .csv
-}
ensureCSVExtension :: String -> String
ensureCSVExtension fileName =
    if ".csv" `isSuffixOf` fileName
        then fileName
        else fileName ++ ".csv"

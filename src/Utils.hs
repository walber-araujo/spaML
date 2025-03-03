{-# LANGUAGE DeriveGeneric #-}

module Utils where

-- Funções utilitárias (como leitura de arquivos, manipulação de dados)

import Data.Csv
import Data.Maybe ( fromMaybe )
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import GHC.Generics
import System.Info (os)
import System.IO (hFlush, stdout)
import System.Process (callCommand)
import qualified Data.Map as Map
import qualified Data.Aeson as Aeson
import System.IO (withFile, IOMode(..), appendFile)
import System.Directory (doesFileExist)
import Data.List (isSuffixOf)

-- Defina uma estrutura de dados para as linhas do CSV
data MyRecord = MyRecord
  { label :: String   -- Para o tipo (ham ou spam)
  , message :: String -- Para o conteúdo da mensagem
  } deriving (Show, Generic)

instance FromRecord MyRecord

-- Define o tipo para armazenar os modelos
type ModelMap = Map.Map String FilePath

-- Função para ler o arquivo CSV
readCSV :: FilePath -> IO (Either String (V.Vector MyRecord))
readCSV filePath = do
  csvData <- BL.readFile filePath
  return $ decodeWith defaultDecodeOptions HasHeader csvData

-- Função para dividir os dados em treinamento e teste
divideDataset :: V.Vector MyRecord -> (V.Vector MyRecord, V.Vector MyRecord)
divideDataset records = 
    let total = V.length records
        trainSize = (total * 3) `div` 10 -- 70% para treinamento
    in V.splitAt trainSize records

divideCsvTrainingTest :: FilePath -> V.Vector MyRecord -> IO (V.Vector MyRecord, V.Vector MyRecord)
divideCsvTrainingTest filePath records = do
                if filePath == "data/train_data/SMSSpamCollection.csv"
                then do
                     return (divideDataset records)
                else do 
                     vectorCsvDefault <- downloadDefault
                     return (records, vectorCsvDefault)

downloadDefault :: IO (V.Vector MyRecord)
downloadDefault = do
                    fileCsvDefault <- BL.readFile "data/train_data/SMSSpamCollection.csv"
                    let registrosDefault = decode HasHeader fileCsvDefault :: Either String (V.Vector MyRecord)
                    case registrosDefault of
                        Left err -> return V.empty
                        Right rgsDefault -> return rgsDefault

clearTerminal :: IO ()
clearTerminal = do
    let command = if os == "mingw32" then "cls" else "clear"
    callCommand command

flushOutput :: IO ()
flushOutput = hFlush stdout

saveToCSV :: FilePath -> String -> String -> IO()
saveToCSV fileName classification message = do
  let csvLine = classification ++ "," ++ message ++ "\n"
  appendFile fileName csvLine 

  clearTerminal
  putStrLn "Data saved successfully!\n"

-- Função para carregar o JSON contendo os modelos
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

-- Exibir modelos disponíveis
printModels :: ModelMap -> IO ()
printModels models = do
    mapM_ putStrLn (Map.keys models)

-- Função para atualizar e salvar a lista de modelos
saveModelToJSON :: String -> FilePath -> IO ()
saveModelToJSON modelName filePath = do
    let jsonPath = "./data/models/models.json"

    -- Tenta carregar o arquivo de modelos
    existingModels <- loadModelMap jsonPath
    let updatedModels = Map.insert modelName filePath existingModels
    BL.writeFile jsonPath (Aeson.encode updatedModels)
    
    putStrLn "\n✅ Model saved successfully!"

ensureCSVExtension :: String -> String
ensureCSVExtension fileName =
    if ".csv" `isSuffixOf` fileName
        then fileName
        else fileName ++ ".csv"

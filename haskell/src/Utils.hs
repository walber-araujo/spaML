{-# LANGUAGE DeriveGeneric #-}

module Utils where

-- Funções utilitárias (como leitura de arquivos, manipulação de dados)

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Classifier
import GHC.Generics

-- Defina uma estrutura de dados para as linhas do CSV
data MyRecord = MyRecord
  { label :: String   -- Para o tipo (ham ou spam)
  , message :: String -- Para o conteúdo da mensagem
  } deriving (Show, Generic)

instance FromRecord MyRecord

-- Função para ler o arquivo CSV
readCSV :: FilePath -> IO (Either String (V.Vector MyRecord))
readCSV filePath = do
  csvData <- BL.readFile filePath
  return $ decode HasHeader csvData

-- Função para dividir os dados em treinamento e teste
divideDataset :: V.Vector MyRecord -> (V.Vector MyRecord, V.Vector MyRecord)
divideDataset records = 
    let total = V.length records
        trainSize = total `div` 5 -- 20% para treinamento
    in V.splitAt trainSize records
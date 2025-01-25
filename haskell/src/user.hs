{-# LANGUAGE DeriveGeneric #-}

-- Importação de módulos 

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import GHC.Generics
import qualified Data.Vector as V
import qualified Data.Map as Map
import Data.List (words, foldl')
import Control.Monad (forM_)
import System.IO (hFlush, stdout)
import Text.Printf (printf)

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

-- Função para preprocessar o texto (tokenização simples)
tokenize :: String -> [String]
tokenize = words . map (\c -> if c `elem` ['a'..'z'] ++ ['A'..'Z'] then c else ' ')

-- Função para contar as palavras por classe (ham ou spam)
countWords :: V.Vector MyRecord -> Map.Map String Int -> Map.Map String Int -> (Int, Int) -> (Map.Map String Int, Map.Map String Int, Int, Int)
countWords records hamWords spamWords (hamCount, spamCount) = 
    V.foldl' updateCounts (hamWords, spamWords, hamCount, spamCount) records
  where
    updateCounts (hWords, sWords, hCount, sCount) record =
      let tokens = tokenize (message record)
          lbl = label record
          (newHWords, newSWords, newHCount, newSCount) =
            if lbl == "ham" then
                (foldl' (\acc word -> Map.insertWith (+) word 1 acc) hWords tokens, sWords, hCount + 1, sCount)
            else
                (hWords, foldl' (\acc word -> Map.insertWith (+) word 1 acc) sWords tokens, hCount, sCount + 1)
      in (newHWords, newSWords, newHCount, newSCount)

-- Função para calcular a probabilidade de uma palavra para cada classe
calculateWordProbabilities :: Map.Map String Int -> Int -> Map.Map String Int -> Int -> Map.Map String Double -> Map.Map String Double
calculateWordProbabilities wordsCount totalCount otherWordsCount otherCount wordProbs =
    Map.foldrWithKey updateProb wordProbs wordsCount
  where
    updateProb word count acc =
        let pWordGivenClass = (fromIntegral count + 1) / (fromIntegral totalCount + fromIntegral (Map.size wordsCount)) -- Laplace smoothing
            otherCountWord = Map.findWithDefault 0 word otherWordsCount
            pOtherClassGivenWord = (fromIntegral otherCountWord + 1) / (fromIntegral otherCount + fromIntegral (Map.size otherWordsCount)) -- Laplace smoothing
        in Map.insert word (pWordGivenClass / pOtherClassGivenWord) acc

-- Função para classificar uma nova mensagem
classifyMessage :: Map.Map String Double -> Map.Map String Double -> String -> Double
classifyMessage hamProbs spamProbs msg =
    let tokens = tokenize msg
        hamProb = sum [Map.findWithDefault 0 word hamProbs | word <- tokens]
        spamProb = sum [Map.findWithDefault 0 word spamProbs | word <- tokens]
    in if hamProb > spamProb then 0 else 1 -- 0 = ham, 1 = spam

-- Função para treinar o modelo
trainModel :: V.Vector MyRecord -> (Map.Map String Double, Map.Map String Double, Int, Int)
trainModel records = 
    let (hamWords, spamWords, hamCount, spamCount) = countWords records Map.empty Map.empty (0, 0)
        hamProbs = calculateWordProbabilities hamWords hamCount spamWords spamCount Map.empty
        spamProbs = calculateWordProbabilities spamWords spamCount hamWords hamCount Map.empty
    in (hamProbs, spamProbs, hamCount, spamCount)

-- Função para testar o modelo e calcular a acurácia
testModel :: V.Vector MyRecord -> Map.Map String Double -> Map.Map String Double -> IO Double
testModel records hamProbs spamProbs = do
    let correct = foldl' (\acc record -> 
                            let result = classifyMessage hamProbs spamProbs (message record)
                            in if (result == 0 && label record == "ham") || (result == 1 && label record == "spam") then acc + 1 else acc) 
                         0 records
        total = V.length records
    return (fromIntegral correct / fromIntegral total)

-- Função principal que será executada
main :: IO ()
main = do
    -- Carregar o arquivo CSV
    arquivoCSV <- BL.readFile "/home/joao/Haskell/SMSSpamCollection.csv"
    
    -- Fazer o parsing do CSV
    let registros = decode HasHeader arquivoCSV :: Either String (V.Vector MyRecord)
    
    -- Verificar o resultado e exibir
    case registros of
        Left err -> putStrLn $ "Erro ao ler o CSV: " ++ err
        Right rgs -> do
            -- Treinar o modelo
            let (hamProbs, spamProbs, _, _) = trainModel rgs
            -- Calcular a acurácia
            accuracy <- testModel rgs hamProbs spamProbs
            putStrLn $ "Acurácia do modelo: " ++ printf "%.2f" (accuracy * 100) ++ "%"
            
            -- Solicitar ao usuário uma nova mensagem para classificar
            putStrLn "Digite uma mensagem para classificar (ou 'sair' para encerrar):"
            loop hamProbs spamProbs

-- Função para loop de entrada do usuário
loop :: Map.Map String Double -> Map.Map String Double -> IO ()
loop hamProbs spamProbs = do
    putStr "> "
    hFlush stdout
    msg <- getLine
    if msg == "sair"
        then putStrLn "Programa encerrado."
        else do
            let result = classifyMessage hamProbs spamProbs msg
            putStrLn $ "A mensagem foi classificada como: " ++ if result == 0 then "ham" else "spam"
            loop hamProbs spamProbs

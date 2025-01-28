-- Função principal que será executada

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import Text.Printf (printf)
import Utils
import Training
import ModelTest
import CLI

--import Data.Csv

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
            -- Dividir o dataset em treino e teste
            let (trainSet, testSet) = divideDataset rgs

            -- Treinar o modelo
            let (hamProbs, spamProbs, _, _) = trainModel trainSet

            -- Calcular a acurácia no conjunto de teste
            accuracy <- testModel testSet hamProbs spamProbs
            putStrLn $ "Acurácia do modelo no conjunto de teste: " ++ printf "%.2f" (accuracy * 100) ++ "%"
            
            -- Solicitar ao usuário uma nova mensagem para classificar
            putStrLn "Digite uma mensagem para classificar (ou 'sair' para encerrar):"
            loop hamProbs spamProbs
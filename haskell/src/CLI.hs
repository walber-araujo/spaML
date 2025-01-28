module CLI where

-- Implementação da interface de linha de comando

import qualified Data.Map as Map
import System.IO (hFlush, stdout)
import Classifier

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
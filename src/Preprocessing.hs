module Preprocessing where
    
-- Funções de pré-processamento de texto (limpeza, tokenização, etc.)

-- Função para preprocessar o texto (tokenização simples)
tokenize :: String -> [String]
tokenize = words . map (\c -> if c `elem` ['a'..'z'] ++ ['A'..'Z'] then c else ' ')
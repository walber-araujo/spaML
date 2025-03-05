module Preprocessing where

import Data.Char (toLower)
-- Funções de pré-processamento de texto (limpeza, tokenização, etc.)

-- Lista de stop words em inglês
stopWordsEn :: [String]
stopWordsEn = ["i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves", 
               "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them", "their", 
               "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are", 
               "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "a", "an", 
               "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", 
               "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", 
               "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", 
               "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", 
               "nor", "not", "only", "own", "same", "so", "than", "too", "very", "s", "t", "can", "will", "just", "don", 
               "should", "now", "d", "ll", "m", "o", "re", "ve", "y", "ain", "aren", "couldn", "didn", "doesn", "hadn", 
               "hasn", "haven", "isn", "ma", "mightn", "mustn", "needn", "shan", "shouldn", "wasn", "weren", "won", "wouldn"]

-- Função para preprocessar o texto (tokenização simples)
{- |
    List with stop words in English.
    Parameters:
        - '[String]': stop words in English.
-}
tokenize :: String -> [String]
tokenize = filter (`notElem` stopWordsEn) . map (map toLower) . words . map (\c -> if c `elem` ['a'..'z'] ++ ['A'..'Z'] then c else ' ')
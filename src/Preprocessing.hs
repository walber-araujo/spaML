module Preprocessing where

{-
Module      : Preprocessing
Description : Preprocessing of text (cleaning and tokenization).
Stability   : stable.
-}    

{- |
    Preprocessing the text (simple tokenization).
    Parameters:
        - 'String': word to preprocess.
    Return:
        - '[String]': word after the preprocess.
-}
tokenize :: String -> [String]
tokenize = words . map (\c -> if c `elem` ['a'..'z'] ++ ['A'..'Z'] then c else ' ')
module ModelTest where

{-
Module      : ModelTest
Description : Function to test the model and calculate accuracy.
Stability   : stable.
-}

import qualified Data.Vector as V
import qualified Data.Map as Map
import Data.List (foldl')
import Classifier
import Utils

{- |
    Test the model by classifying each message and calculate the accuracy.
    Parameters:
        - 'V.Vector MyRecord': words.
        - 'Map.Map String Double': probability of be ham. 
        - 'Map.Map String Double': probability of be spam.

    Return:
        - 'IO Double': ratio between correct classifications and total records.
-}
testModel :: V.Vector MyRecord -> Map.Map String Double -> Map.Map String Double -> IO Double
testModel records hamProbs spamProbs = do
    let correct = foldl' (\acc record -> 
                            let result = classifyMessage hamProbs spamProbs (message record)
                            in if (result == 0 && label record == "ham") || (result == 1 && label record == "spam") then acc + 1 else acc) 
                         0 records
        total = V.length records
    return (fromIntegral correct / fromIntegral total)
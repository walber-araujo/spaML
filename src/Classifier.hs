module Classifier where

{-|
Module      : Classifier
Description : Classification logic to determine whether a message is 'spam' or 'ham'.
Stability   : stable
-}

import qualified Data.Map as Map
import Preprocessing

{-|
    Classifies a new message as 'ham' (0) or 'spam' (1) based on the probabilities of the words.

    The function calculates the sum of the word probabilities present in the message for both 'ham' and 'spam' categories and returns:
    - 0 if the message is classified as 'ham'.
    - 1 if the message is classified as 'spam'.

    Parameters:
      - `hamProbs` : A map (`Map.Map String Double`) containing the probabilities of words for 'ham' messages.
      - `spamProbs` : A map (`Map.Map String Double`) containing the probabilities of words for 'spam' messages.
      - `msg` : The message to be classified (of type `String`).

    Return:
      - `Double` : 0 for 'ham', 1 for 'spam'.
-}
classifyMessage :: Map.Map String Double -> Map.Map String Double -> String -> Double
classifyMessage hamProbs spamProbs msg =
    let tokens = tokenize msg
        hamProb = sum [Map.findWithDefault 0 word hamProbs | word <- tokens]
        spamProb = sum [Map.findWithDefault 0 word spamProbs | word <- tokens]
    in if hamProb > spamProb then 0 else 1 -- 0 = ham, 1 = spam
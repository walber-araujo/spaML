import Utils
import CLI
import Intro

-- Função principal que será executada
main :: IO ()
main = do
    clearTerminal
    showIntro
    clearTerminal
    putStrLn "Welcome to spaML!"
    putStrLn "This project performs spam detection using Machine Learning with the Naive Bayes algorithm."
    putStrLn "The code was developed in Haskell by Alex, João, Vinícius, and Walber."
    waitForAnyKey
    menu
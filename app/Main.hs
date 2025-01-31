-- Função principal que será executada
import Utils
import CLI

main :: IO ()
main = do
    clearTerminal
    putStrLn "Welcome to spaML!"
    putStrLn "This project performs spam detection using Machine Learning with the Naive Bayes algorithm."
    putStrLn "The code was developed in Haskell by Alex, João, Vinícius, and Walber."
    putStrLn "It was created for the Programming Language Paradigms course at UFCG.\n"
    menu
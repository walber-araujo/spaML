module CLI where 

-- Implementação da interface de linha de comando

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B
import System.IO (hFlush, stdout)
import Training
import Classifier
import Utils
import Control.Monad (forever)
import System.Exit (exitSuccess)

-- Menu interativo
menu :: IO ()
menu = do
    putStrLn "Menu Options:\n"
    putStrLn "1. Train model with categorized files"
    putStrLn "2. Reuse previous models"
    putStrLn "3. Train model manually"
    putStrLn "4. Classify individual messages using the default model"
    putStrLn "5. Show results with accuracy rates"
    putStrLn "6. Exit"
    putStr "\nChoose an option: "
    hFlush stdout

    option <- getLine
    processOption option

processOption :: String -> IO ()
processOption option = case option of
    "1" -> do
        clearTerminal
        putStr "Enter the name to the CSV file to train the model: "
        hFlush stdout
        fileName <- getLine

        putStrLn ""

        (hamProbs, spamProbs) <- trainModelCSV ("./data/train_data/" ++ fileName)
        classificationSubmenu hamProbs spamProbs

        menu
    "2" -> do
        clearTerminal
        reusingPreviousModelSubmenu

        menu
    "3" -> do
        clearTerminal
        putStrLn "Training model manually...\n"

        putStr "Enter the file name: "
        hFlush stdout
        fileName <- getLine 
        let filePath = "./data/train_data/" ++ fileName

        clearTerminal 
        trainingManualSubmenu filePath

        (hamProbs, spamProbs) <- trainModelCSV filePath
        menu
    "4" -> do
        clearTerminal
        putStrLn "\nClassifying individual messages...\n"
        (hamProbs, spamProbs) <- trainModelCSV "data/train_data/SMSSpamCollection.csv" 
        classificationSubmenu hamProbs spamProbs
    "5" -> do
        clearTerminal
        putStrLn "\nShowing results with accuracy rates...\n"
        -- Implementar exibição de resultados
        menu
    "6" -> do
        putStrLn "\nExiting...\n"
        exitSuccess
    _ -> do
        clearTerminal
        putStrLn "\nInvalid option. Please try again.\n"
        menu

-- Submenu para classificação de mensagens individuais
classificationSubmenu :: Map.Map String Double -> Map.Map String Double -> IO ()
classificationSubmenu hamProbs spamProbs = do
    clearTerminal
    putStrLn "Classification Submenu:\n"
    putStrLn "1. Classify a message"
    putStrLn "2. Return to main menu"
    putStr "\nChoose an option: "
    hFlush stdout

    option <- getLine
    case option of
        "1" -> do
            clearTerminal
            putStrLn "Type a message to classify (or 'exit' to quit):"
            loop hamProbs spamProbs

        "2" -> do
            clearTerminal
            putStrLn "\nReturning to main menu...\n"
            menu

        _ -> do
            putStrLn "Invalid option. Please try again."
            classificationSubmenu hamProbs spamProbs

trainingManualSubmenu :: FilePath -> IO()
trainingManualSubmenu filePath = do
    putStrLn "Training Manual Submenu:\n"

    putStr "Enter the classification of the message (spam or ham) or 'exit' to stop: "
    hFlush stdout
    classification <- getLine

    if classification == "exit"
        then do
            clearTerminal
            putStrLn "Exiting training manual submenu\n"
        else do
            putStr "\nEnter the message: "
            hFlush stdout
            message <- getLine

            saveToCSV filePath classification message
            trainingManualSubmenu filePath

    -- After training, ask for a model name and save it
    putStr "\nEnter a name for this model: "
    hFlush stdout
    modelName <- getLine
    saveModelToJSON modelName filePath

-- Função para loop de entrada do usuário
loop :: Map.Map String Double -> Map.Map String Double -> IO ()
loop hamProbs spamProbs = do
    putStr "> "
    hFlush stdout
    msg <- getLine
    if msg == "exit"
        then do 
            clearTerminal
            menu
        else do
            let result = classifyMessage hamProbs spamProbs msg
            putStrLn $ "The message has been classified as: " ++ if result == 0 then "ham" else "spam"
            loop hamProbs spamProbs

-- Submenu for reusing models
reusingPreviousModelSubmenu :: IO ()
reusingPreviousModelSubmenu = do
    let jsonPath = "./data/models/models.json" 
    modelMap <- loadModelMap jsonPath
    case modelMap of
        models -> do
            putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            putStrLn "       Available Models       "
            putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            printModels models
            putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            putStr "\nEnter the name of the model you want to reuse: "
            hFlush stdout
            modelName <- getLine
            case Map.lookup modelName models of
                Just csvPath -> do
                    putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
                    putStrLn $ "  Training with model: " ++ modelName
                    putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
                    (hamProbs, spamProbs) <- trainModelCSV csvPath
                    classificationSubmenu hamProbs spamProbs

                    clearTerminal
                    menu
                Nothing -> do
                    putStrLn "\n⚠️  Model not found. Please try again."
                    reusingPreviousModelSubmenu

module CLI where 

-- Implementação da interface de linha de comando

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B
import System.IO (hFlush, stdout)
import Training
import Classifier
import Utils
import Metric
import Intro
import Control.Monad (forever)
import System.Exit (exitSuccess)

-- Menu interativo
menu :: IO ()
menu = do
    clearTerminal
    putStrLn "\n=========================================="
    putStrLn "Menu Options:\n"
    putStrLn "1. Train model with categorized files"
    putStrLn "2. Reuse previous models"
    putStrLn "3. Train model manually"
    putStrLn "4. Classify individual messages using the default model"
    putStrLn "5. Show results with accuracy rates"
    putStrLn "6. Exit"
    putStr "\nChoose an option (1-6): "
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

        putStr "Enter the file name (type with .csv): "
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
        accuracyCSVs "data/train_data"
        waitForAnyKey
        menu

    "6" -> do
        showOut
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

trainingManualLoop :: FilePath -> IO ()
trainingManualLoop filePath = do
    saveToCSV filePath "Label" "Message"

    putStrLn "First, enter the spam messages: "

    saveMessagesTraining filePath "spam"

    putStrLn "\nNow, enter the ham messages: "

    saveMessagesTraining filePath "ham"

saveMessagesTraining :: FilePath -> String -> IO()   
saveMessagesTraining filePath classification = do
               putStr "Enter the message (or 'exit'): "
               hFlush stdout
               message <- getLine

               if message == "exit" then return ()
               else do
                    saveToCSV filePath classification message
                    saveMessagesTraining filePath classification

trainingManualSubmenu :: FilePath -> IO ()
trainingManualSubmenu filePath = do
    putStrLn "Training Manual Submenu:\n"
    
    trainingManualLoop filePath  

    putStr "\nEnter a name for this model: "
    hFlush stdout
    modelName <- getLine
    saveModelToJSON modelName filePath  

    clearTerminal
    putStrLn "Training manual completed and model saved.\n"

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

module CLI where 

-- Implementação da interface de linha de comando

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B
import Training
import Classifier
import Utils
import Metric
import Intro
import Control.Monad (forever)
import System.Exit (exitSuccess)
import System.Directory (doesFileExist)

-- Menu interativo
menu :: IO ()
menu = do
    clearTerminal
    putStrLn "\n=========================================="
    putStrLn "Menu Options:\n"
    putStrLn "1. Reuse previous models"
    putStrLn "2. Add new model"
    putStrLn "3. Train model manually"
    putStrLn "4. Classify individual messages using the default model"
    putStrLn "5. Show results with accuracy rates"
    putStrLn "6. Exit"
    putStr "\nChoose an option (1-6): "
    flushOutput

    option <- getLine
    processOption option

processOption :: String -> IO ()
processOption option = case option of
    "1" -> do
        clearTerminal
        reusingPreviousModelSubmenu

        menu

    "2" -> do
        clearTerminal
        -- TODO: adicionar texto explicando a função
        putStrLn "\nAdd a new model by providing a name and selecting a CSV file containing training data."
        putStrLn "Type a name to your model (or 'exit' to quit).\n"
        addNewModelSubmenu

        menu 

    "3" -> do
        clearTerminal
        putStrLn "Training model manually...\n"
        putStr "Enter the file name or type 'exit' to return: "
        flushOutput
        modelName <- getLine

        if modelName == "exit"
            then menu
            else do
                let fullPath = getCSVFilePath modelName
                
                clearTerminal
                trainingManualSubmenu fullPath modelName

                putStrLn "\nDo you want to save this model? (y/n): "
                flushOutput
                confirmation <- getLine

                if confirmation == "y"
                    then do
                        saveModelToJSON modelName fullPath
                        putStrLn "Model saved successfully!"
                        menu
                    else do
                        putStrLn "Model was not saved."
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

getCSVFilePath :: String -> FilePath
getCSVFilePath modelName = "./data/train_data/" ++ ensureCSVExtension modelName

-- Submenu para classificação de mensagens individuais
classificationSubmenu :: Map.Map String Double -> Map.Map String Double -> IO ()
classificationSubmenu hamProbs spamProbs = do
    clearTerminal
    putStrLn "Classification Submenu:\n"
    putStrLn "1. Classify a message"
    putStrLn "2. Return to main menu"
    putStr "\nChoose an option: "
    flushOutput

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
    putStrLn "Enter spam messages first. Type 'exit' to move to ham messages.\n"
    collectMessages "spam" 

    clearTerminal
    putStrLn "Now enter ham messages. Type 'exit' to stop.\n"
    collectMessages "ham"  
  where
    collectMessages classification = do
        putStr $ "Enter a " ++ classification ++ " message (or 'exit' to stop): "
        flushOutput
        message <- getLine

        if message == "exit"
            then return ()
            else do
                saveToCSV filePath classification message
                collectMessages classification  
                clearTerminal

trainingManualSubmenu :: FilePath -> String-> IO ()
trainingManualSubmenu filePath  modelName = do
    putStrLn "Training Manual Submenu:\n"
    
    trainingManualLoop filePath  

    clearTerminal
    saveModelToJSON modelName filePath  

    clearTerminal
    putStrLn "Training manual completed and model saved.\n"

-- Função para loop de entrada do usuário
loop :: Map.Map String Double -> Map.Map String Double -> IO ()
loop hamProbs spamProbs = do
    putStr "> "
    flushOutput
    msg <- getLine
    if msg == "exit"
        then do 
            clearTerminal
            menu
        else do
            let result = classifyMessage hamProbs spamProbs msg
            putStrLn $ "The message has been classified as: " ++ if result == 0 then "ham" else "spam"
            loop hamProbs spamProbs

askPath :: IO String
askPath = do
    putStr "Enter the model path: "
    flushOutput
    modelPath <- getLine
    fileExists <- doesFileExist modelPath
    if not fileExists then do
        putStrLn $ "\n⚠️  Model path \"" ++ modelPath ++ "\" not found. Please try again."
        askPath
    else return modelPath

addNewModelSubmenu :: IO()
addNewModelSubmenu = do
    putStr "Enter the new model name: "
    flushOutput
    modelName <- getLine

    if modelName == "exit"
        then menu
        else do
            modelPath <- askPath
            saveModelToJSON modelName modelPath

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
            putStr "\nEnter the name of the model you want to reuse (or 'exit' to quit): "
            flushOutput
            modelName <- getLine
            if modelName == "exit"
                then do
                    clearTerminal
                    menu
                else case Map.lookup modelName models of
                    Just csvPath -> do
                        fileExists <- doesFileExist csvPath
                        if fileExists
                            then do
                                putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
                                putStrLn $ "  Training with model: " ++ modelName
                                putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
                                (hamProbs, spamProbs) <- trainModelCSV csvPath
                                classificationSubmenu hamProbs spamProbs

                                clearTerminal
                                menu
                            else do
                                clearTerminal
                                putStrLn $ "\n⚠️  CSV file " ++ csvPath ++ " not found. Please check the file path."
                                reusingPreviousModelSubmenu

                    Nothing -> do
                        clearTerminal
                        putStrLn $ "\n⚠️  Model " ++ modelName ++ " not found. Please try again."
                        reusingPreviousModelSubmenu


module CLI where 

-- Implementação da interface de linha de comando

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as Aeson
import Training
import Classifier
import Utils
import Metric
import Intro
import Control.Monad (forever)
import System.Exit (exitSuccess)
import System.Directory (doesFileExist, removeFile)

-- Menu interativo
menu :: IO ()
menu = do
    clearTerminal
    putStrLn "\n=========================================="
    putStrLn "Menu Options:\n"
    putStrLn "1. Reuse previous models."
    putStrLn "2. Add new model."
    putStrLn "3. Remove a model."
    putStrLn "4. Train model manually."
    putStrLn "5. Classify individual messages using the default model."
    putStrLn "6. Show results with accuracy rates."
    putStrLn "7. Exit."
    putStr "\nChoose an option (1-7): "
    flushOutput

    option <- getLine
    processOption option

-- Processa a opção escolida pelo usuário
processOption :: String -> IO ()
processOption option = case option of
    "1" -> do
        clearTerminal
        reusingPreviousModelSubmenu

        menu

    "2" -> do
        clearTerminal
        putStrLn "\nAdd a new model by providing a name and selecting a CSV file containing training data."
        putStrLn "Type a name to your model (or 'exit' to quit).\n"
        addNewModelSubmenu

        menu
    
    "3" -> do
        clearTerminal
        removeModelSubmenu
        menu

    "4" -> do
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

    "5" -> do
        clearTerminal
        putStrLn "\nClassifying individual messages...\n"
        (hamProbs, spamProbs) <- trainModelCSV "data/train_data/SMSSpamCollection.csv" 
        classificationSubmenu hamProbs spamProbs

    "6" -> do
        clearTerminal
        putStrLn "\nShowing results with accuracy rates...\n"
        accuracyCSVs "data/train_data"
        waitForAnyKey
        menu

    "7" -> do
        showOut
        exitSuccess

    _ -> do
        clearTerminal
        putStrLn "\nInvalid option. Please try again.\n"
        menu

-- Retorna o path do arquivo CSV garantindo que tenha a extensão .csv
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

-- Recebe as mensagens spam depois as ham e as adiciona no arquivo para treinamento manual
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

-- Certifica-se de salvar ou modelo ou apagá-lo a depender da escolha do usuário
trainingManualSubmenu :: FilePath -> String -> IO ()
trainingManualSubmenu filePath modelName = do
    putStrLn "Training Manual Submenu:\n"
    
    trainingManualLoop filePath  -- Coleta as mensagens (spam e ham)

    clearTerminal
    putStrLn "\nDo you want to save this model? (y/n): "
    flushOutput
    confirmation <- getLine

    if confirmation == "y"
        then do
            saveModelToJSON modelName filePath
            clearTerminal
            putStrLn "Model saved successfully."
            menu
        else do
            -- Se o usuário não confirmar, removemos o arquivo CSV
            removeFile filePath
            clearTerminal
            putStrLn "Model was not saved and the data file has been removed."
            menu

-- Função para loop de entrada de mensagens a serem classificadas
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

-- Recebe o path do modelo a ser adicionado 
askPath :: IO String
askPath = do
    putStr "Enter the model path (or 'exit' to quit): "
    flushOutput
    modelPath <- getLine
    fileExists <- doesFileExist modelPath
    if modelPath == "exit" 
        then return "unknown"
    else do 
        if not fileExists then do
            clearTerminal
            putStrLn $ "\n⚠️  Model path \"" ++ modelPath ++ "\" not found. Please try again."
            askPath
        else return modelPath

-- Adiciona novo modelo
addNewModelSubmenu :: IO()
addNewModelSubmenu = do
    putStr "Enter the new model name: "
    flushOutput
    modelName <- getLine

    if modelName == "exit"
        then menu
        else do
            modelPath <- askPath
            if modelPath == "unknown" then return()
            else saveModelToJSON modelName modelPath

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

-- Remove um modelo existente com exceção dos modelos default
removeModelSubmenu :: IO ()
removeModelSubmenu = do
    let jsonPath = "./data/models/models.json"
    modelMap <- loadModelMap jsonPath
    if Map.null modelMap
        then do
            putStrLn "\nNo models found to remove."
            waitForAnyKey
        else do
            putStrLn "\n━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            putStrLn "       Available Models       "
            putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            printModels modelMap
            putStrLn "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
            putStr "\nEnter the name of the model to remove (or 'exit' to cancel): "
            flushOutput
            modelName <- getLine

            if modelName == "exit" then
                return ()
            else if modelName `elem` ["modelo1", "modelo2"] then do
                putStrLn $ "\n⚠️  Model '" ++ modelName ++ "' cannot be removed as it is default model of the system."
                waitForAnyKey
                removeModelSubmenu
            else case Map.lookup modelName modelMap of
                Just _ -> do
                    let updatedModels = Map.delete modelName modelMap
                    BL.writeFile jsonPath (Aeson.encode updatedModels)
                    putStrLn $ "\nModel '" ++ modelName ++ "' removed successfully!"
                    waitForAnyKey
                Nothing -> do
                    putStrLn $ "\nModel '" ++ modelName ++ "' not found. Please try again."
                    removeModelSubmenu
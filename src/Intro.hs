module Intro where

import System.IO (hFlush, stdout, hSetBuffering, stdin, BufferMode(NoBuffering, LineBuffering), hReady)
import Control.Concurrent (threadDelay)
import Control.Monad (unless, void)
import Utils (clearTerminal)

-- Função que exibe um texto com efeito de digitação
typeWriter :: String -> IO ()
typeWriter [] = return ()
typeWriter (c:cs) = do
    putChar c
    hasInput <- hReady stdin
    if hasInput
        then return ()
    else do
        hFlush stdout
        threadDelay 24000  -- Tempo de atraso em microssegundos (40ms por caractere)
        typeWriter cs

-- Exibe cada linha do logo de forma animada
animatedLogo :: IO ()
animatedLogo = do
    let logo =
          [ "  █████    ██████    █████    ███    ███  ██       "
          , " █         ██   ██  ██   ██   ████  ████  ██       "
          , "   ███     ██████   ███████   ██ ████ ██  ██       "
          , "      ██   ██       ██   ██   ██  ██  ██  ██       "
          , "  ████     ██       ██   ██   ██      ██  ███████  "
          ]

    loop logo
    where
        loop :: [String] -> IO ()
        loop [] = return ()
        loop (line:left) = do
            typeWriter (line ++ "\n")
            hasInput <- hReady stdin
            if hasInput then do -- lê o caractere inserido e limpa o terminal
                void getChar
                clearTerminal
            else do
                threadDelay 100000 -- 200ms entre cada linha
                loop left

-- Aguarda qualquer tecla pressionada para continuar
waitForAnyKey :: IO ()
waitForAnyKey = do
    hSetBuffering stdin NoBuffering  -- Desativa o buffer da entrada padrão
    putStrLn "\nPress any key to continue..."
    hFlush stdout
    _ <- getChar  -- Captura qualquer tecla pressionada
    putStr "\b \b"  -- Apaga a tecla pressionada
    hSetBuffering stdin LineBuffering
    return ()

-- Exibe uma introdução animada ao sistema antes do menu principal
showIntro :: IO ()
showIntro = do
    putStrLn "\n"
    hSetBuffering stdin NoBuffering
    animatedLogo  -- Exibe o nome do software com animação
    hSetBuffering stdin LineBuffering
    putStrLn "\n=========================================="
    typeWriter "       Welcome to S P A M L Classifier    \n"
    putStrLn "=========================================="
    typeWriter "\nThis program helps you classify messages as spam or ham.\n"
    typeWriter "You can train a model, classify messages, and check accuracy results.\n"
    waitForAnyKey

-- Exibe uma mensagem final ao sair do sistema com efeito de digitação
showOut :: IO ()
showOut = do
    putStrLn "\n=========================================="
    typeWriter "   Thank you for using S P A M L!   \n"
    putStrLn "=========================================="
    typeWriter "\nGoodbye!\n"
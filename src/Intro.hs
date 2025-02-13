module Intro where

import System.IO (hFlush, stdout)
import Control.Concurrent (threadDelay)

-- Função que exibe um texto com efeito de digitação
typeWriter :: String -> IO ()
typeWriter [] = return ()
typeWriter (c:cs) = do
    putChar c
    hFlush stdout
    threadDelay 30000  -- Tempo de atraso em microssegundos (40ms por caractere)
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
    mapM_ (\line -> typeWriter (line ++ "\n") >> threadDelay 100000) logo  -- 200ms entre cada linha

-- Exibe uma introdução animada ao sistema antes do menu principal
showIntro :: IO ()
showIntro = do
    putStrLn "\n"
    animatedLogo  -- Exibe o nome do software com animação
    putStrLn "\n=========================================="
    typeWriter "       Welcome to S P A M L Classifier    \n"
    putStrLn "=========================================="
    typeWriter "\nThis program helps you classify messages as spam or ham.\n"
    typeWriter "You can train a model, classify messages, and check accuracy results.\n"
    typeWriter "\nPress Enter to continue..."
    hFlush stdout
    _ <- getLine  -- Aguarda o usuário pressionar Enter
    return ()

-- Exibe uma mensagem final ao sair do sistema com efeito de digitação
showOut :: IO ()
showOut = do
    putStrLn "\n=========================================="
    typeWriter "   Thank you for using S P A M L!   \n"
    putStrLn "=========================================="
    typeWriter "\nGoodbye!\n"
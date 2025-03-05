module Intro where

{-|
Module      : Intro
Description : Provides animated intro and outro with typing effect for the SPA ML Classifier program.
Stability   : stable
-}

import System.IO (hFlush, stdout, hSetBuffering, stdin, BufferMode(NoBuffering, LineBuffering), hReady)
import Control.Concurrent (threadDelay)
import Control.Monad (when, void)
import Utils (clearTerminal)


{-|
    Simulates a typing effect by printing each character with a small delay.
    The typing stops if the user presses any key, otherwise it continues to display the string.

    Parameters:
      - `msg` : The string to be displayed with the typing effect.

    Return:
      - `IO ()` : Action that produces the typing effect.
-}
typeWriter' :: String -> IO ()
typeWriter' [] = return ()
typeWriter' (c:cs) = do
    hasInput <- hReady stdin
    if hasInput
        then return ()
    else do
        putChar c
        hFlush stdout
        threadDelay 24000  -- Tempo de atraso em microssegundos (24ms por caractere)
        typeWriter' cs

-- retorna verdadeiro caso não tenha sido pressionada nenhuma tecla durante a animação
typeWriter :: String -> IO Bool
typeWriter s = do
    hSetBuffering stdin NoBuffering
    typeWriter' s
    hasInput <- hReady stdin
    hSetBuffering stdin LineBuffering
    if hasInput then do
        getChar
        clearTerminal
        return False
    else
        return True

{-|
    Displays the system's logo with an animated typing effect. 
    Each line of the logo is displayed one by one, with a delay in between.
    The user can interrupt the animation by pressing any key, which will clear the terminal.

    Return:
      - `IO ()` : Action that displays the animated logo.
-}
animatedLogo :: IO ()
animatedLogo = do
    let logo =
          [ "  █████    ██████    █████    ███    ███  ██       "
          , " ██        ██   ██  ██   ██   ████  ████  ██       "
          , "   ███     ██████   ███████   ██ ████ ██  ██       "
          , "      ██   ██       ██   ██   ██  ██  ██  ██       "
          , "  ████     ██       ██   ██   ██      ██  ███████  "
          ]

    loop logo
    where
        loop :: [String] -> IO ()
        loop [] = return ()
        loop (line:left) = do
            b <- typeWriter (line ++ "\n")
            when b (do
                threadDelay 100000 -- 100ms entre cada linha
                loop left)

{-|
    Waits for the user to press any key to continue. The input is not buffered, and the program will proceed
    as soon as any key is pressed.

    Return:
      - `IO ()` : Action that waits for the user to press a key.
-}
waitForAnyKey :: IO ()
waitForAnyKey = do
    hSetBuffering stdin NoBuffering  -- Desativa o buffer da entrada padrão
    putStrLn "\nPress any key to continue..."
    hFlush stdout
    _ <- getChar  -- Captura qualquer tecla pressionada
    putStr "\b \b"  -- Apaga a tecla pressionada
    hSetBuffering stdin LineBuffering
    return ()

{-|
    Displays the introductory animation with the system's logo and a welcome message.
    After the animation, it shows a brief description of the program's functionality
    and waits for the user to press any key before proceeding.

    Return:
      - `IO ()` : Action that shows the introduction and waits for user input.
-}
showIntro :: IO ()
showIntro = do
    putStrLn "\n"
    animatedLogo  -- Exibe o nome do software com animação
    putStrLn "\n=========================================="
    typeWriter "       Welcome to S P A M L Classifier    \n"
    putStrLn "=========================================="
    typeWriter "\nThis program helps you classify messages as spam or ham.\n"
    typeWriter "You can train a model, classify messages, and check accuracy results.\n"
    waitForAnyKey

{-|
    Displays a final message with a typing effect when the user exits the program.
    The message expresses gratitude and bids farewell to the user.

    Return:
      - `IO ()` : Action that shows the final message before exiting.
-}
showOut :: IO ()
showOut = do
    putStrLn "\n=========================================="
    typeWriter "   Thank you for using S P A M L!   \n"
    putStrLn "=========================================="
    typeWriter "\nGoodbye!\n"
    return ()

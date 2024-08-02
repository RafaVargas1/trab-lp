module Game (startGame) where

import Player (playerMove, computerMoveEasy, computerMoveHard)
import Utils (initializeDice, displayDice, Dice)

startGame :: IO ()
startGame = do
    putStrLn "Bem-vindo (a) ao jogo do dado!"
    putStrLn "Informe o número de dados: "
    n <- readLn
    dice <- initializeDice n
    putStrLn "Selecione o nivel de dificuldade:\n(1) - Fácil\n(2) - Difícil"
    level <- readLn
    playGame dice level True

playGame :: [Dice] -> Int -> Bool -> IO ()
playGame dice level isPlayerTurn = do
    displayDice dice
    if null dice then
        putStrLn $ if isPlayerTurn then "Vitória da máquina!" else "Vitória do jogador!"
    else if isPlayerTurn then do
        newDice <- playerMove dice
        playGame newDice level False
    else do
        newDice <- if level == 1 then computerMoveEasy dice else computerMoveHard dice
        playGame newDice level True
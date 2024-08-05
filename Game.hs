--Arthur Vieira da Silva - 202035013
--Rafael de Oliveira Vargas - 202035022

module Game (startGame) where

import Player (playerMove)
import Computer (computerMove)
import Utils (initializeDice, displayDice, Dice)

chooseDifficult :: [Dice] -> IO ()
chooseDifficult dice = do    
    putStrLn "Qual será a dificuldade?"
    putStrLn "Digite 1 para: [Fácil]"
    putStrLn "Digite 2 para: [Difícil]"
    level <- readLn
    if level > 2 then do       
        putStrLn "Entrada inválida!!!"
        chooseDifficult dice
    else do
        if level == 2 then do
            playGame dice level False
        else do
            playGame dice level True

startGame :: IO ()
startGame = do
    putStrLn "Começando o jogo dos dados!"
    putStrLn "Quantos serão os dados?"
    n <- readLn
    dice <- initializeDice n
    chooseDifficult dice

playGame :: [Dice] -> Int -> Bool -> IO ()
playGame dice level isPlayerTurn = do
    displayDice dice
    if null dice then
        putStrLn $ if isPlayerTurn then "Derrota!!!" else "Vitória!!!"
    else if isPlayerTurn then do
        newDice <- playerMove dice
        playGame newDice level False
    else do
        newDice <- computerMove dice level
        putStrLn "Vez da máquina"
        playGame newDice level True
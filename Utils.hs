--Arthur Vieira da Silva - 202035013
--Rafael de Oliveira Vargas - 202035022

module Utils (initializeDice, displayDice, getNewDiceValue, setDice, rotateDice, removeDice, Dice) where

import System.Random (randomRIO)

type Dice = Int

initializeDice :: Int -> IO [Dice]
initializeDice n = do
    allDices <- sequence (replicate n (randomRIO (1, 6)))
    return allDices

displayDice :: [Dice] -> IO ()
displayDice dice = putStrLn $ "Estado do jogo: " ++ show dice

getNewDiceValue :: Int -> Int -> Int
getNewDiceValue actualDice side
    | actualDice == 6 && side == 1 = 3
    | actualDice == 6 && side == 2 = 2
    | actualDice == 6 && side == 3 = 4
    | actualDice == 6 && side == 4 = 5
    | actualDice == 5 && side == 1 = 3  
    | actualDice == 5 && side == 2 = 6
    | actualDice == 5 && side == 3 = 4
    | actualDice == 5 && side == 4 = 1
    | actualDice == 4 && side == 1 = 1
    | actualDice == 4 && side == 2 = 5
    | actualDice == 4 && side == 3 = 6
    | actualDice == 4 && side == 4 = 2
    | actualDice == 3 && side == 1 = 1  
    | actualDice == 3 && side == 2 = 2
    | actualDice == 3 && side == 3 = 6
    | actualDice == 3 && side == 4 = 5
    | actualDice == 2 && side == 1 = 3
    | actualDice == 2 && side == 2 = 1
    | actualDice == 2 && side == 3 = 4
    | actualDice == 2 && side == 4 = 6
    | actualDice == 1 && side == 1 = 3  
    | actualDice == 1 && side == 2 = 5
    | actualDice == 1 && side == 3 = 4
    | actualDice == 1 && side == 4 = 2

setDice :: Int -> Int -> [Dice] -> [Dice]
setDice idx nValue dice = take idx dice ++ [nValue] ++ drop (idx +1) dice

rotateDice :: Int -> Int -> [Dice] -> [Dice]
rotateDice idx side dice =
    let actualDice = dice !! idx
    in take idx dice ++ [getNewDiceValue actualDice side] ++ drop (idx + 1) dice

removeDice :: Int -> [Dice] -> [Dice]
removeDice idx dice = take idx dice ++ drop (idx + 1) dice
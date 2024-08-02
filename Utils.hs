module Utils (initializeDice, displayDice, rotateDice, removeDice, Dice) where

import System.Random (randomRIO)

type Dice = Int

initializeDice :: Int -> IO [Dice]
initializeDice n = do
    -- Gera todos os N dados (Só é necessário saber um dos lados do dado)
    allDices <- sequence (replicate n (randomRIO (1, 6)))
    return allDices

displayDice :: [Dice] -> IO ()
displayDice dice = putStrLn $ "Dado atual: " ++ show dice

rotateDice :: Int -> [Dice] -> [Dice]
rotateDice idx dice =
    let newVal = if dice !! idx > 1 then dice !! idx - 1 else 1
    in take idx dice ++ [newVal] ++ drop (idx + 1) dice

removeDice :: Int -> [Dice] -> [Dice]
removeDice idx dice = take idx dice ++ drop (idx + 1) dice
module Player (playerMove) where

import System.Random (randomRIO)
import Utils (Dice, rotateDice, removeDice, getNewDiceValue)

chooseDice :: [Dice] -> IO Int
chooseDice dice = do    
    putStrLn "Sua vez, qual dado será jogado?"
    position <- readLn :: IO Int
    let index = position - 1

    if index > length dice-1 then do
        putStrLn "Entrada inválida!!!"
        chooseDice dice
    else do
        return index

chooseSide :: [Dice] -> Int -> IO Int
chooseSide dice idx = do
    let actualDice = dice !! idx

    let aSides = [getNewDiceValue actualDice side < actualDice | side <- [1..4]]
    let pSides = [i | (i, value) <- zip [1..4] aSides, value]

    case length pSides of
        1 -> return (pSides !! 0)
        2 -> do
            putStrLn "Qual face ficará pra cima?"
            putStrLn $ "Digite 1 para a face: [" ++ show (getNewDiceValue actualDice (pSides !! 0)) ++ "]"
            putStrLn $ "Digite 2 para a face: [" ++ show (getNewDiceValue actualDice (pSides !! 1)) ++ "]"
            processChoice pSides
        3 -> do
            putStrLn "Qual face ficará pra cima?"
            putStrLn $ "Digite 1 para a face: [" ++ show (getNewDiceValue actualDice (pSides !! 0)) ++ "]"
            putStrLn $ "Digite 2 para a face: [" ++ show (getNewDiceValue actualDice (pSides !! 1)) ++ "]"
            putStrLn $ "Digite 3 para a face: [" ++ show (getNewDiceValue actualDice (pSides !! 2)) ++ "]"
            processChoice pSides
        4 -> do
            putStrLn "Qual face ficará pra cima?"
            putStrLn $ "Digite 1 para a face: [" ++ show (getNewDiceValue actualDice (pSides !! 0)) ++ "]"
            putStrLn $ "Digite 2 para a face: [" ++ show (getNewDiceValue actualDice (pSides !! 1)) ++ "]"
            putStrLn $ "Digite 3 para a face: [" ++ show (getNewDiceValue actualDice (pSides !! 2)) ++ "]"
            putStrLn $ "Digite 4 para a face: [" ++ show (getNewDiceValue actualDice (pSides !! 3)) ++ "]"
            processChoice pSides

  where
    processChoice :: [Int] -> IO Int
    processChoice pSides = do
        cSide <- readLn :: IO Int
        
        if cSide <= length pSides then do            
            let side = pSides !! (cSide - 1)
            return side
        else do
            putStrLn "Entrada inválida!!!"
            chooseSide dice idx

playerMove :: [Dice] -> IO [Dice]
playerMove dice = do
    index <- chooseDice dice
    let idx = index
    if dice !! idx == 1 then
        return (removeDice idx dice)
    else do
        cSide <- chooseSide dice idx
        let side = cSide
        return (rotateDice idx side dice)
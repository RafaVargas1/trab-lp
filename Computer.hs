module Computer (computerMove) where

import System.Random (randomRIO)
import Utils (Dice, rotateDice, removeDice, getNewDiceValue)

singleDiceWin :: Dice -> Bool
singleDiceWin 1 = True
singleDiceWin 2 = False
singleDiceWin 3 = True
singleDiceWin 4 = True
singleDiceWin 5 = False
singleDiceWin 6 = True

twoDiceWin :: Dice -> Dice -> Bool
twoDiceWin a b
    | a == b = False
    | a + b == 7 = False
    | otherwise = True

manyDiceWin :: [Dice] -> Bool
manyDiceWin dice
    | all (`elem` [2, 5]) dice = False
    | otherwise = not (canMakeLosingPairs (filter (`notElem` [2, 5]) dice))

canMakeLosingPairs :: [Dice] -> Bool
canMakeLosingPairs [] = True
canMakeLosingPairs (x:xs) = any (not . (`twoDiceWin` x)) xs && canMakeLosingPairs xs

winnerConfig :: [Dice] -> Bool
winnerConfig [] = False
winnerConfig [a] = singleDiceWin a
winnerConfig [a, b] = twoDiceWin a b 
winnerConfig dList = manyDiceWin dList

findValidRotation :: Int -> Dice -> [Dice] -> [Dice] -> Maybe Dice
findValidRotation _ _ [] _ = Nothing
findValidRotation _ _ _ [] = Nothing
findValidRotation pos original (x:xs) dice
    | pos >= length dice = Nothing
    | otherwise = findValidRotation' pos original xs dice
  where
    findValidRotation' _ _ [] _ = Nothing
    findValidRotation' _ _ _ [] = Nothing
    findValidRotation' pos original (x:xs) dice
        | x < original && not (winnerConfig (rotateDice pos x dice)) = Just x
        | otherwise = findValidRotation' pos original xs dice

findDiceToRotate :: [Dice] -> Int -> Maybe (Int, Dice)
findDiceToRotate [] _ = Nothing
findDiceToRotate (x:xs) idx =
    case findValidRotation idx x [getNewDiceValue x side | side <- [1..4]] (x:xs) of
        Just rotation -> Just (idx, rotation)
        Nothing -> findDiceToRotate xs (idx + 1)

computerDice :: [Dice] -> IO [Dice]
computerDice dice = do
        let diceToRotate = findDiceToRotate dice 0
        case diceToRotate of
            Just (idx, rotation) -> return (take idx dice ++ [rotation] ++ drop (idx + 1) dice)
            Nothing -> return [-1]
            
computerSide :: Int -> IO Int
computerSide actualDice = do
    let aSides = [getNewDiceValue actualDice side < actualDice | side <- [1..4]]
    let pSides = [i | (i, value) <- zip [1..4] aSides, value]
    
    sIndex <- randomRIO(1, length pSides)
    let sIdx = sIndex - 1

    let side = pSides !! sIdx

    return side

computerMove :: [Dice] -> Int -> IO [Dice]
computerMove dice level = do
    if level == 1 then do 
        idx <- randomRIO(0, length dice-1)
        if dice !! idx == 1 then do
            return (removeDice idx dice)
        else do 
            let actualDice = dice !! idx
            cSide <- computerSide actualDice
            let side = cSide
            return (rotateDice idx side dice)
    else do
        let onesList = map (== 1) dice
        let onesIndex = [i | (i, value) <- zip [0..] onesList, value]
        
        if not (null onesIndex) then do 
            putStrLn "remove"
            return (removeDice (onesIndex !! 0) dice)
        else do            
            putStrLn "replace"
            nDice <- computerDice dice
            if (nDice !! 0) /= -1 then do
                putStrLn "~random"
                return nDice
            else do 
                putStrLn "random"
                idx <- randomRIO(0, length dice-1)
                if dice !! idx == 1 then do
                    return (removeDice idx dice)
                else do 
                    let actualDice = dice !! idx
                    cSide <- computerSide actualDice
                    let side = cSide
                    return (rotateDice idx side dice)
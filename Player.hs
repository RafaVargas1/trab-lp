module Player (playerMove, computerMoveEasy, computerMoveHard) where

import Utils (Dice, rotateDice, removeDice)

playerMove :: [Dice] -> IO [Dice]
playerMove dice = do
    putStrLn "Seu turno! Escolha um dado para tirar ou rotacionar: "
    idx <- readLn
    if dice !! idx == 1 then
        return (removeDice idx dice)
    else do
        putStrLn "Rotacionar ou tirar?\nR - Rotacionar\nT - Tirar"
        action <- getLine
        if action == "r" then
            return (rotateDice idx dice)
        else
            return (removeDice idx dice)

computerMoveEasy :: [Dice] -> IO [Dice]
computerMoveEasy dice = do
    let idx = head [i | i <- [0..length dice-1], dice !! i /= 1]
    if dice !! idx == 1 then
        return (removeDice idx dice)
    else
        return (rotateDice idx dice)

computerMoveHard :: [Dice] -> IO [Dice]
computerMoveHard dice = do
    -- Implementar lógica para jogada perfeita
    return dice
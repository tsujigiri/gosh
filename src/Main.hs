import System.IO
import qualified Data.Map.Strict as Map
import Go
import Go.UI.Color

gameLoop :: Game -> IO ()
gameLoop game = do
    printPrompt game
    input <- getLine
    case processInput input game of
        Right updatedGame -> do
            print updatedGame
            gameLoop updatedGame
        Left error -> do
            putStrLn error
            gameLoop game

processInput :: String -> Game -> Either String Game
processInput "pass" game = pass game
processInput (x:y) game = do
    parsedX <- case Map.lookup x coordLetters of
                   Just parsedX -> Right parsedX
                   Nothing -> Left "Invalid input"
    parsedY <- case reads y of
                   [(parsedY, "")] -> Right parsedY
                   _ -> Left "Invalid input"
    addMove (Point (parsedX, parsedY)) game

printPrompt :: Game -> IO ()
printPrompt game = do
    whoseMove <- return $ nextStone game
    whoseMoveLiterally <- return $ case whoseMove of
                                       Black -> "Black"
                                       White -> "White"
    putStr $ whoseMoveLiterally ++ " to move: "
    hFlush stdout

main = do
    game <- return $ newGame
    print game
    gameLoop game

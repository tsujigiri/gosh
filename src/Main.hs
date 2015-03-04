import Go
import Control.Monad.Trans.Maybe
import qualified Data.Map.Strict as Map

gameLoop :: Game -> IO ()
gameLoop game = do
    print game
    input <- getLine
    case processInput input game of
        Right updatedGame -> gameLoop updatedGame
        Left error -> do
            putStrLn error
            gameLoop game

processInput :: String -> Game -> Either String Game
processInput "pass" game = pass game
processInput (x:y) game = do
    parsedX <- case Map.lookup x coordLetters of
                   Just parsedX -> Right parsedX
                   Nothing -> Left "Invalid input"
    parsedY <- Right (read y::Int)
    addMove game $ Point (parsedX, parsedY)

main = gameLoop newGame

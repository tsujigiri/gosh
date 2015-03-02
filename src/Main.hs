import Go
import Control.Monad.Trans.Maybe

gameLoop :: Game -> IO ()
gameLoop game = do
    print game
    coords <- runMaybeT getCoords
    case coords of
        Nothing -> do
            putStrLn "Invalid input."
            gameLoop game
        Just point ->
            case addMove game point of
                Left error -> do
                    putStrLn error
                    gameLoop game
                Right newGame ->
                    gameLoop newGame

getCoords :: MaybeT IO Point
getCoords = MaybeT $ do
    input <- getLine
    return $ parseCoords input


main = gameLoop newGame

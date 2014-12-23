import Go

gameLoop game = do
  print game
  gameLoop . addMove game . parseCoords =<< getLine

main = gameLoop newGame



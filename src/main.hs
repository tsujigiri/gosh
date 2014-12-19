import Go

game_loop game = do
  print game
  game_loop . add_move game . parse_coords =<< getLine

main = game_loop new_game



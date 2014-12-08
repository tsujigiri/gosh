module Go (new_game, add_move) where

import qualified Data.Map.Strict as Map

data Point = Point (Int, Int) deriving (Show, Ord, Eq)
data Stone = Black | White deriving Show
data Game = Game {
  board :: Map.Map Point Stone,
  moves :: [Point],
  size :: Int
} deriving Show

new_game :: Game
new_game = Game {
  board = Map.empty,
  moves = [],
  size = 19
}

add_move :: Game -> Point -> Game
add_move (Game { moves = moves, board = board, size = size }) (Point (x, y))
  | x <= size && y <= size =
    Game {
      moves = (Point (x, y)):moves,
      board = (Map.insert (Point (x, y)) (next_stone moves) board),
      size = size
    }

next_stone :: [Point] -> Stone
next_stone moves
  | even (length moves) = Black
  | otherwise = White


module Go (new_game, add_move) where

import qualified Data.Map.Strict as Map

data X = A | B | C | D | E | F | G | H | J | K | L | M | N | O | P | Q | R | S | T deriving (Show, Ord, Eq)
type Y = Int
type Coordinate = (X, Y)
data Stone = Black | White deriving Show
data Game = Game {
  board :: Map.Map Coordinate Stone,
  moves :: [Coordinate]
} deriving Show

new_game :: Game
new_game = Game {
  board = Map.empty,
  moves = []
}

add_move :: Game -> Coordinate -> Game
add_move (Game { moves = moves, board = board }) move =
  Game { moves = move:moves, board = (Map.insert move (next_stone moves) board) }

next_stone :: [Coordinate] -> Stone
next_stone moves
  | even (length moves) = Black
  | otherwise = White


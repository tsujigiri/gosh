module Go (new_game, add_move) where

import qualified Data.Map.Strict as Map
import Data.List

data Point = Point (Int, Int) deriving (Show, Ord, Eq)
data Stone = Black | White | Ko deriving Eq
data Game = Game {
  board :: Map.Map Point Stone,
  moves :: [Point],
  size :: Int
}

instance Show Stone where
  show Black = "○"
  show White = "●"
  show Ko = "□"

instance Show Game where
  show game@Game { size = size } =
    intercalate "\n" [ concat [draw_board game (Point (x, y)) | x <- [1..size] ] | y <- [1..size] ]

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

draw_board :: Game -> Point -> String
draw_board (Game { board = board, size = size }) point
  | (Map.lookup point board) == Nothing = [board_at size point]
  | otherwise = show (definitely (Map.lookup point board))

board_at :: Int -> Point -> Char
board_at size (Point (x, y))
  | x == 1 && y == 1 = '┌'
  | x == 1 && y == size = '└'
  | x == size && y == 1 = '┐'
  | x == size && y == size = '┘'
  | y == 1 = '┬'
  | y == size = '┴'
  | x == 1 = '├'
  | x == size = '┤'
  | otherwise = '┼'

definitely :: Maybe a -> a
definitely (Just a) = a


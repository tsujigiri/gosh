module Go (new_game, add_move, parse_coords) where

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
add_move (Game { moves = moves, board = board, size = size }) point@(Point (x, y))
  | x <= size && y <= size =
    Game {
      moves = point:moves,
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

parse_coords :: String -> Point
parse_coords (x:y) = Point ((definitely (Map.lookup x coord_letters)), read y)

coord_letters :: Map.Map Char Int
coord_letters = Map.fromList [
    ('a', 1), ('b', 2), ('c', 3), ('d', 4), ('e', 5), ('f', 6), ('g', 7),
    ('h', 8), ('j', 9), ('k', 10), ('l', 11), ('m', 12), ('n', 13), ('o', 14),
    ('p', 15), ('q', 16), ('r', 17), ('s', 18), ('t', 19)
  ]


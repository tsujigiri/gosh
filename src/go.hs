module Go (newGame, addMove, parseCoords) where

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
        showXCoords
        ++ "\n"
        ++ concat [
            concat (
                    [showYCoord y]
                    ++ [drawBoard game (Point (x, y)) | x <- [1..size] ]
                    ++ [" "]
                    ++ [showYCoord y]
                    ++ ["\n"]
            ) | y <- [1..size]
        ] ++ showXCoords

newGame :: Game
newGame = Game {
    board = Map.empty,
    moves = [],
    size = 19
}

addMove :: Game -> Point -> Game
addMove game@(Game { moves = moves, board = board, size = size }) point@(Point (x, y))
    | validMove =
        Game {
        moves = point:moves,
        board = Map.insert point (nextStone moves) board,
        size = size
    }
    where validMove = x <= size && x >= 1 && y <= size && y >= 1
                      && boardAt game point == Nothing

nextStone :: [Point] -> Stone
nextStone moves
    | even (length moves) = Black
    | otherwise = White

boardAt :: Game -> Point -> Maybe Stone
boardAt (Game { board = board }) point = Map.lookup point board

drawBoard :: Game -> Point -> String
drawBoard game@(Game { size = size }) point
    | stone == Nothing = [gridAt size point]
    | otherwise = show $ definitely stone
    where stone = boardAt game point

gridAt :: Int -> Point -> Char
gridAt size (Point (x, y))
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

parseCoords :: String -> Point
parseCoords (x:y) = Point ((definitely (Map.lookup x coordLetters)), read y)

coordLetters :: Map.Map Char Int
coordLetters = Map.fromList [
        ('a', 1), ('b', 2), ('c', 3), ('d', 4), ('e', 5), ('f', 6), ('g', 7),
        ('h', 8), ('j', 9), ('k', 10), ('l', 11), ('m', 12), ('n', 13),
        ('o', 14), ('p', 15), ('q', 16), ('r', 17), ('s', 18), ('t', 19)
    ]

showYCoord :: Int -> String
showYCoord y = (replicate (2 - length coord) ' ') ++ coord ++ " "
    where coord = show y

showXCoords :: String
showXCoords = "   " ++ Map.keys coordLetters

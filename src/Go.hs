module Go (
    newGame,
    addMove,
    pass,
    coordLetters,
    nextStone,

    Game(..),
    Point(..),
    Stone(..),
    boardAt
) where

import qualified Data.Map.Strict as Map
import Control.Applicative

data Point = Point (Int, Int) deriving (Show, Ord, Eq)
data Stone = Black | White | Ko deriving Eq
data Game = Game {
    board :: Map.Map Point Stone,
    moves :: [Maybe Point],
    size :: Int
} deriving Eq

newGame :: Game
newGame = Game {
    board = Map.empty,
    moves = [],
    size = 19
}

addMove :: Game -> Point -> Either String Game
addMove game point = do
    return game
    >>= validateCoords point
    >>= insertMove point
    >>= removeCaptured

pass :: Game -> Either String Game
pass game@Game { moves = moves } = Right game { moves = Nothing:moves }

validateCoords :: Point -> Game -> Either String Game
validateCoords point@(Point (x, y)) game@(Game { size = size })
    | invalidCoords = Left "Invalid coordinates"
    | taken = Left "Point taken"
    | otherwise = Right game
    where invalidCoords = x > size || x < 1 || y > size || y < 1
          taken = boardAt game point /= Nothing

insertMove :: Point -> Game -> Either String Game
insertMove point game@(Game { moves = moves, board = board }) =
    Right $ game {
        moves = (Just point):moves,
        board = Map.insert point (nextStone game) board
    }

nextStone :: Game -> Stone
nextStone Game { moves = moves }
    | even (length moves) = Black
    | otherwise = White

boardAt :: Game -> Point -> Maybe Stone
boardAt (Game { board = board }) point = Map.lookup point board

unwrap :: Maybe a -> a
unwrap (Just a) = a

coordLetters :: Map.Map Char Int
coordLetters = Map.fromList [
        ('a', 1), ('b', 2), ('c', 3), ('d', 4), ('e', 5), ('f', 6), ('g', 7),
        ('h', 8), ('j', 9), ('k', 10), ('l', 11), ('m', 12), ('n', 13),
        ('o', 14), ('p', 15), ('q', 16), ('r', 17), ('s', 18), ('t', 19)
    ]

collectGroup :: Game -> Point -> [Maybe Point] -> [Maybe Point]
collectGroup game@(Game { size = size }) point@(Point (x, y)) seen
    | x < 1 || y < 1 || x > size || y > size = seen
    | currentPoint == Just Ko || currentPoint == Nothing = Nothing:seen
    | elem (pure point) seen = seen
    | seen == [] || head(seen) == Nothing || currentPoint == boardAt game (unwrap . head $ seen) =
        groupInGame up
        . groupInGame down
        . groupInGame left
        . groupInGame right $ (pure point):seen
    | otherwise = seen
    where currentPoint = boardAt game point
          up = Point (x, y - 1)
          down = Point (x, y + 1)
          left = Point (x - 1, y)
          right = Point (x + 1, y)
          groupInGame = collectGroup game

deadGroup :: [Maybe Point] -> [Point]
deadGroup groupPoints
    | any (== Nothing) groupPoints = []
    | otherwise = map unwrap groupPoints

removeCaptured :: Game -> Either String Game
removeCaptured game@Game { board = board, moves = moves }
    | head moves == Nothing = Right game
    | otherwise =
        Right $ game { board =
           multiDelete (deadGroup upperGroup)
           $ multiDelete (deadGroup lowerGroup)
           $ multiDelete (deadGroup leftGroup)
           $ multiDelete (deadGroup rightGroup) board
        }

    where up = Point (x, y - 1)
          down = Point (x, y + 1)
          left = Point (x - 1, y)
          right = Point (x + 1, y)
          upperGroup = collectGroup game up []
          lowerGroup = collectGroup game down []
          leftGroup = collectGroup game left []
          rightGroup = collectGroup game right []
          Just (Point (x, y)):_ = moves

multiDelete :: Ord a => [a] -> Map.Map a b -> Map.Map a b
multiDelete (key:keys) map = multiDelete keys $ Map.delete key map
multiDelete [] map = map


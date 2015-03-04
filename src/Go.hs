module Go (
    newGame,
    addMove,
    pass,
    parseCoords,

    Game(..),
    Point(..),
    Stone(..),
    boardAt
) where

import qualified Data.Map.Strict as Map
import Data.List
import Control.Applicative
import Colors

data Point = Point (Int, Int) deriving (Show, Ord, Eq)
data Stone = Black | White | Ko deriving Eq
data Game = Game {
    board :: Map.Map Point Stone,
    moves :: [Maybe Point],
    size :: Int
} deriving Eq

instance Show Stone where
    show Black = "●"
    show White = fgWhite ++ "●" ++ fgBlack
    show Ko = "□"

instance Show Game where
    show game@Game { size = size } =
        showXCoords
        ++ "\n"
        ++ concat [
            concat (
                    [showYCoord y]
                    ++ [boardColors]
                    ++ [drawBoard game (Point (x, y)) | x <- [1..size] ]
                    ++ [reset]
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

boardColors :: String
boardColors = fgBlack ++ bgYellow

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
        board = Map.insert point (nextStone moves) board
    }

nextStone :: [Maybe Point] -> Stone
nextStone moves
    | even (length moves) = Black
    | otherwise = White

boardAt :: Game -> Point -> Maybe Stone
boardAt (Game { board = board }) point = Map.lookup point board


drawBoard :: Game -> Point -> String
drawBoard game@(Game { size = size }) point
    | stone == Nothing = gridAt size point
    | otherwise = (show (unwrap stone)) ++ (tail (gridAt size point))
    where stone = boardAt game point

gridAt :: Int -> Point -> String
gridAt size (Point (x, y))
    | x == 1 && y == 1 = "┌─"
    | x == 1 && y == size = "└─"
    | x == size && y == 1 = "┐"
    | x == size && y == size = "┘"
    | y == 1 = "┬─"
    | y == size = "┴─"
    | x == 1 = "├─"
    | x == size = "┤"
    | otherwise = "┼─"

unwrap :: Maybe a -> a
unwrap (Just a) = a

parseCoords :: String -> Maybe Point
parseCoords (x:y) = do
    parsedX <- Map.lookup x coordLetters
    parsedY <- Just $ read y
    return $ Point (parsedX, parsedY)

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
showXCoords = "   " ++ (intersperse ' ' $ Map.keys coordLetters)


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


module Go (
    newGame,
    addMove,
    pass,
    coordLetters,
    nextStone,
    boardAt,
    score,
    markDead,

    Game(..),
    Point(..),
    Stone(..)
) where

import Data.List
import qualified Data.Map.Lazy as Map
import Control.Applicative
import Control.Monad.State
import Control.Monad.List
import Data.Maybe

data Point = Point (Int, Int) deriving (Show, Ord, Eq)
data Stone = Empty | Black | White | Ko deriving Eq
type Board = Map.Map Point Stone
data Game = Game {
    board :: Board,
    moves :: [Maybe Point],
    size :: Int,
    dead :: [Point]
} deriving Eq

data SegmentAndAdjacent = SegmentAndAdjacent {
    segment :: Board,
    segmentType :: Stone
} deriving Eq

newGame :: Game
newGame = Game {
    board = Map.fromList [ ((Point (x, y)), Empty) | x <- [1..19], y <- [1..19] ],
    moves = [],
    size = 19,
    dead = []
}

newSegment :: Stone -> SegmentAndAdjacent
newSegment segmentType = SegmentAndAdjacent {
    segment = Map.empty,
    segmentType = segmentType
    }

addMove :: Point -> Game -> Either String Game
addMove point game = do
    return game
    >>= validateCoords point
    >>= clearKo
    >>= checkGameOver
    >>= insertMove point
    >>= removeCapturedNeighbors

pass :: Game -> Either String Game
pass game = do
    return game
    >>= clearKo
    >>= checkGameOver
    >>= \game' -> Right game' { moves = Nothing:(moves game) }

validateCoords :: Point -> Game -> Either String Game
validateCoords point game
    | isOffBoard point game = Left "Invalid coordinates"
    | taken = Left "Invalid move"
    | otherwise = Right game
    where Game { size = size } = game
          Point (x, y) = point
          taken = boardAt game point /= Just Empty

insertMove :: Point -> Game -> Either String Game
insertMove point game =
    Right $ game {
        moves = (Just point):moves,
        board = updateBoard point (nextStone game) board
    }
    where Game { moves = moves, board = board } = game

clearKo :: Game -> Either String Game
clearKo game =
    Right $ game { board = Map.map emptyKo board }
        where Game { board = board } = game
              emptyKo Ko = Empty
              emptyKo a = a

checkGameOver :: Game -> Either String Game
checkGameOver Game { moves = Nothing:Nothing:_ } = Left "Game over"
checkGameOver game = Right game

nextStone :: Game -> Stone
nextStone Game { moves = moves }
    | even (length moves) = Black
    | otherwise = White

boardAt :: Game -> Point -> Maybe Stone
boardAt game point = Map.lookup point board
    where Game { board = board } = game

coordLetters :: Map.Map Char Int
coordLetters = Map.fromList [
        ('a', 1), ('b', 2), ('c', 3), ('d', 4), ('e', 5), ('f', 6), ('g', 7),
        ('h', 8), ('j', 9), ('k', 10), ('l', 11), ('m', 12), ('n', 13),
        ('o', 14), ('p', 15), ('q', 16), ('r', 17), ('s', 18), ('t', 19)
    ]

removeCapturedNeighbors :: Game -> Either String Game
removeCapturedNeighbors game@Game { board = board, moves = moves }
    | head moves == Nothing = Right game
    | otherwise = Right $ foldl removeCaptured game [up, down, left, right]
    where up = Point (x, y - 1)
          down = Point (x, y + 1)
          left = Point (x - 1, y)
          right = Point (x + 1, y)
          Just (Point (x, y)):_ = moves

removeCaptured :: Game -> Point -> Game
removeCaptured game point
    | isOffBoard point game = game
    | nextStone game /= currentStone = game
    | length dead == 1 = game { board = Map.insert point Ko board }
    | otherwise = game { board = multiInsert dead Empty board }
    where dead = deadGroup $ collectSegmentAt game (newSegment currentStone) point
          Just currentStone = boardAt game point
          Game { board = board, size = size } = game
          Point (x, y) = point

collectSegmentAt :: Game -> SegmentAndAdjacent -> Point -> SegmentAndAdjacent
collectSegmentAt game segmentAndAdjacent point
    | isOffBoard point game = segmentAndAdjacent
    | Map.member point segment = segmentAndAdjacent
    | currentStone == segmentType =
        foldl (collectSegmentAt game) segmentAndAdjacent' [up, down, left, right]
    | otherwise = segmentAndAdjacent { segment = updateBoard point currentStone segment }
    where Game { size = size, dead = dead } = game
          SegmentAndAdjacent { segment = segment, segmentType = segmentType } =
              segmentAndAdjacent
          Just currentStone = boardAt game point
          segmentAndAdjacent' = segmentAndAdjacent {
              segment = updateBoard point currentStone segment,
              segmentType = segmentType
              }
          Point (x, y) = point
          up = Point (x, y - 1)
          down = Point (x, y + 1)
          left = Point (x - 1, y)
          right = Point (x + 1, y)

deadGroup :: SegmentAndAdjacent -> [Point]
deadGroup SegmentAndAdjacent { segment = segment, segmentType = segmentType }
    | any (== Empty) (Map.elems segment) = []
    | otherwise = Map.foldMapWithKey collectEquals segment
        where collectEquals k v = if v == segmentType then [k]
                                                      else []

multiInsert :: Ord k => [k] -> v -> Map.Map k v -> Map.Map k v
multiInsert ks v m = foldl (\m' k -> Map.insert k v m') m ks

updateBoard :: Point -> Stone -> Board -> Board
updateBoard point stone = Map.insert point stone

markDead :: Point -> Game -> Game
markDead point game = game { dead = point : dead  }
    where Game { dead = dead } = game

emptyPoints :: Board -> [Point]
emptyPoints = Map.keys . Map.filter (== Empty)

allPoints :: Game -> [Point]
allPoints Game { size = size } = do
    x <- [1..size]
    y <- [1..size]
    return $ Point (x, y)

isOffBoard :: Point -> Game -> Bool
isOffBoard point game = x < 1 || y < 1 || x > size || y > size
    where Point (x, y) = point
          Game { size = size } = game

score :: Game -> [(Stone, Int)]
score game = (flip evalState) [] $ runListT $ do
    emptyPoint <- ListT . return $ emptyPoints $ board game
    visitedPoints <- lift get
    guard $ emptyPoint `notElem` visitedPoints
    let currentSegment = collectSegmentAt game (newSegment Empty) emptyPoint
    lift $ modify (emptyPoints (segment currentSegment) ++)
    let maybeOccupier = occupier currentSegment (dead game)
    guard $ maybeOccupier /= Nothing
    let Just occupier = maybeOccupier
    return $ (occupier, count currentSegment (dead game))

occupier :: SegmentAndAdjacent -> [Point] -> Maybe Stone
occupier s dead = if Black `elem` stones && (White `notElem` stones || whiteIsDead) then
                      Just Black
                  else if White `elem` stones && (Black `notElem` stones || blackIsDead) then
                      Just White
                  else
                      Nothing
    where nonEmpty = Map.filter (/= Empty) (segment s)
          stones = Map.elems nonEmpty
          nonEmptyPoints = Map.keys nonEmpty
          occupiedBy stone = Map.keys $ Map.filter (== stone) (segment s)
          whiteIsDead = not . null $ occupiedBy White `intersect` dead
          blackIsDead = not . null $ occupiedBy Black `intersect` dead

count :: SegmentAndAdjacent -> [Point] -> Int
count currentSegment dead = 2 * (length relevantDead) + (length emptyPoints)
    where relevantDead = filter (isInSegment currentSegment) dead
          emptyPoints = filter (Empty ==) . Map.elems . segment $ currentSegment

isInSegment :: SegmentAndAdjacent -> Point -> Bool
isInSegment s p = (p `elem`) . Map.keys . segment $ s

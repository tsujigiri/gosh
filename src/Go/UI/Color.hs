module Go.UI.Color where

import Data.List
import qualified Data.Map.Strict as Map
import ColorCodes
import Go

instance Show Stone where
    show Black = "●"
    show White = fgWhite ++ "●" ++ fgBlack
    show Ko = "□"

instance Show Game where
    show = drawGame

drawGame :: Game -> String
drawGame game = concat [showXCoords, "\n", drawRow game, showXCoords]

drawRow :: Game -> String
drawRow game = do
    y <- [1..(size game)]
    concat [showYCoord y
           ,boardColors
           ,concat [drawBoard game $ Point (x, y) | x <- [1..(size game)] ]
           ,reset
           ," "
           ,showYCoord y
           ,"\n"
           ]

drawBoard :: Game -> Point -> String
drawBoard game point
    | stone == Nothing = gridAt size point
    | otherwise = (show (unwrap stone)) ++ (tail (gridAt size point))
    where stone = boardAt game point
          Game { size = size } = game

boardColors :: String
boardColors = fgBlack ++ bgYellow

showYCoord :: Int -> String
showYCoord y = (replicate (2 - length coord) ' ') ++ coord ++ " "
    where coord = show y

showXCoords :: String
showXCoords = "   " ++ (intersperse ' ' $ Map.keys coordLetters)

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


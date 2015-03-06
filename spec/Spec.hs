import Test.Hspec
import Control.Exception (evaluate)
import qualified Data.Map.Strict as Map

import Go
import Go.UI.Color

main :: IO ()
main = hspec $ do
    describe "Go.addMove" $ do
        it "adds a black stone to an empty board" $ do
            let point = Point (3, 4)
                Right game = addMove newGame point in
                (boardAt game point) `shouldBe` (Just Black)

        it "cannot add a stone to a place already taken" $ do
            let point = Point (3, 4)
                Right game = addMove newGame point in
                (addMove game point) `shouldBe` (Left "Point taken")

        it "cannot add a stone to coordinates outside the board" $ do
            let point = Point (20, 4) in
                (addMove newGame point) `shouldBe` (Left "Invalid coordinates")

        it "removes a single dead stone" $ do
            let moves = ["d4", "d3", "pass", "d5", "pass", "c4", "pass", "e4"]
                game = addMoves moves newGame in
                boardAt game (Point (4, 4)) `shouldBe` Nothing

    describe "Go.pass" $ do
        it "lets the player pass" $ do
            let Right (Game { moves = moves }) = pass newGame in
                moves `shouldBe` [Nothing]

addMoves :: [String] -> Game -> Game
addMoves [] game = game
addMoves ("pass":moves) game = addMoves moves game'
    where Right game' = pass game
addMoves ((x:y):moves) game = addMoves moves game'
    where y' = read y::Int
          Just x' = Map.lookup x coordLetters
          point = Point (x', y')
          Right game' = addMove game point

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
                Right game = addMove point newGame in
                (boardAt game point) `shouldBe` Just Black

        it "cannot add a stone to a place already taken" $ do
            let point = Point (3, 4)
                Right game = addMove point newGame in
                (addMove point game) `shouldBe` (Left "Invalid move")

        it "cannot add a stone to coordinates outside the board" $ do
            let point = Point (20, 4) in
                (addMove point newGame) `shouldBe` (Left "Invalid coordinates")

        it "removes a single dead stone" $ do
            let moves = ["d4", "d3", "pass", "d5", "pass", "c4", "pass", "e4"]
                game = addMoves moves newGame in
                boardAt game (Point (4, 4)) `shouldBe` Just Ko

        it "does not allow to set to a Ko point" $ do
            let moves = ["d4", "d3", "pass", "d5", "pass", "c4", "pass", "e4"]
                game = addMoves moves newGame in
                addMove (Point (4, 4)) game `shouldBe` Left "Invalid move"

        it "clears Ko after one move" $ do
            let moves = ["d4", "d3", "pass", "d5", "pass", "c4", "pass", "e4", "q16"]
                game = addMoves moves newGame in
                boardAt game (Point (4, 4)) `shouldBe` Just Empty

        it "ends the game after second consecutive pass" $ do
            let moves = ["pass", "pass"]
                game = addMoves moves newGame in
                addMove (Point (4, 4)) game `shouldBe` Left "Game over"

    describe "Go.pass" $ do
        it "lets the player pass" $ do
            let Right (Game { moves = moves }) = pass newGame in
                moves `shouldBe` [Nothing]

        it "clears Ko after one pass" $ do
            let moves = ["d4", "d3", "pass", "d5", "pass", "c4", "pass", "e4", "pass"]
                game = addMoves moves newGame in
                boardAt game (Point (4, 4)) `shouldBe` Just Empty

        it "ends the game after second consecutive pass" $ do
            let moves = ["pass", "pass"]
                game = addMoves moves newGame in
                pass game `shouldBe` Left "Game over"

    describe "Go.score" $ do
        it "returns a score" $ do
            let moves = ["a2", "a3", "b2", "b3", "c2", "c3", "c1", "d3", "pass",
                         "d2", "pass", "d1"]
                game = addMoves moves newGame in
                score game `shouldBe` [(Black, 2), (White, 349)]

addMoves :: [String] -> Game -> Game
addMoves [] game = game
addMoves ("pass":moves) game = addMoves moves game'
    where Right game' = pass game
addMoves ((x:y):moves) game = addMoves moves game'
    where y' = read y::Int
          Just x' = Map.lookup x coordLetters
          point = Point (x', y')
          Right game' = addMove point game

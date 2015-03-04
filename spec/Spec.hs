import Test.Hspec
import Control.Exception (evaluate)

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
            let game0 = newGame
                Right game1 = addMove game0 $ Point (4, 4)
                Right game2 = addMove game1 $ Point (4, 3)
                Right game3 = pass game2
                Right game4 = addMove game3 $ Point (4, 5)
                Right game5 = pass game4
                Right game6 = addMove game5 $ Point (3, 4)
                Right game7 = pass game6
                Right game8 = addMove game7 $ Point (5, 4) in
                boardAt game8 (Point (4, 4)) `shouldBe` Nothing

    describe "Go.pass" $ do
        it "lets the player pass" $ do
            let Right (Game { moves = moves }) = pass newGame in
                moves `shouldBe` [Nothing]

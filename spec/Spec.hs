import Test.Hspec
import Control.Exception (evaluate)

import Colors
import Go

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

    describe "Go.pass" $ do
        it "lets the player pass" $ do
            let Right (Game { moves = moves }) = pass newGame in
                moves `shouldBe` [Nothing]

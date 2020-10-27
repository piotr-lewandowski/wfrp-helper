module Test.DiceSpec (spec) where

import Dice
import Test.Hspec
import Text.Parsec

spec :: Spec
spec = parallel $  do
    describe "expression parser for dice" $ do
        describe "parses constants correctly" $ do
            it "1" $
                parse expression "" "1" `shouldBe` (Right $ Constant 1)
            it "234" $
                parse expression "" "234" `shouldBe` (Right $ Constant 234)

        describe "parses single die correctly" $ do
            it "d6" $
                parse expression "" "d6" `shouldBe` (Right $ Single $ Dice 6)
            it "d10" $
                parse expression "" "d10" `shouldBe` (Right $ Single $ Dice 10)

        describe "parses multiple dice correctly" $ do
            it "2d4" $
                parse expression "" "2d4" `shouldBe` (Right $ Multiple $ ManyDice 2 $ Dice 4)
            it "1d3" $
                parse expression "" "1d3" `shouldBe` (Right $ Multiple $ ManyDice 1 $ Dice 3)

        describe "parses sums correctly" $ do
            it "of constants" $
                parse expression "" "1 + 7" `shouldBe` (Right $ Sum (Constant 1) (Constant 7))
            it "of single dice" $
                parse expression "" "d1 + d7" `shouldBe` (Right $ Sum (Single $ Dice 1) (Single $ Dice 7))
            it "of multiple dice" $
                parse expression "" "1d6 + 7d10" `shouldBe` (Right $ Sum (Multiple $ ManyDice 1 $ Dice 6) (Multiple $ ManyDice 7 $ Dice 10))
            it "chain sums" $
                parse expression "" "1 + 7 + 3 + 4" `shouldBe` (Right $ Sum (Constant 1) (Sum (Constant 7) (Sum (Constant 3) (Constant 4))))
            it "mixed expressions" $
                parse expression "" "1 + 7d10" `shouldBe` (Right $ Sum (Constant 1) (Multiple $ ManyDice 7 $ Dice 10))
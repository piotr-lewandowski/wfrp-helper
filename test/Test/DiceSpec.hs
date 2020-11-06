module Test.DiceSpec (spec) where

import Model.Dice
import Language.Dice
import Test.Hspec
import Text.Parsec
import Control.Applicative

spec :: Spec
spec = parallel $  do
    describe "expression parser for dice" $ do
        describe "parses constants correctly" $ do
            it "1" $
                parse diceExpression "" "1" `shouldBe` Right (Constant 1)
            it "234" $
                parse diceExpression "" "234" `shouldBe` Right (Constant 234)

        describe "parses single die correctly" $ do
            it "d6" $
                parse diceExpression "" "d6" `shouldBe` Right (Single $ Dice 6)
            it "d10" $
                parse diceExpression "" "d10" `shouldBe` Right (Single $ Dice 10)

        describe "parses multiple dice correctly" $ do
            it "2d4" $
                parse diceExpression "" "2d4" `shouldBe` Right (Multiple $ ManyDice 2 $ Dice 4)
            it "1d3" $
                parse diceExpression "" "1d3" `shouldBe` Right (Multiple $ ManyDice 1 $ Dice 3)

        describe "parses sums correctly" $ do
            it "of constants" $
                parse diceExpression "" "1 + 7" `shouldBe` Right (Sum (Constant 1) (Constant 7))
            it "of single dice" $
                parse diceExpression "" "d1 + d7" `shouldBe` Right (Sum (Single $ Dice 1) (Single $ Dice 7))
            it "of multiple dice" $
                parse diceExpression "" "1d6 + 7d10" `shouldBe` Right (Sum (Multiple $ ManyDice 1 $ Dice 6) (Multiple $ ManyDice 7 $ Dice 10))
            it "chain sums" $
                parse diceExpression "" "1 + 7 + 3 + 4" `shouldBe` Right (Sum (Constant 1) (Sum (Constant 7) (Sum (Constant 3) (Constant 4))))
            it "mixed expressions" $
                parse diceExpression "" "1 + 7d10" `shouldBe` Right (Sum (Constant 1) (Multiple $ ManyDice 7 $ Dice 10))
    
    describe "eval evaluates values in correct ranges" $ do
        it "evaluates constants" $
            eval (Constant 1) `shouldReturn` 1
        it "evaluates single dice" $ do
            res <- eval (Single $ Dice 6)
            res `shouldSatisfy` liftA2 (&&) (>=1) (<=6)
        it "evaluates multiple dice" $ do
            res <- eval (Multiple $ ManyDice 2 $ Dice 4) 
            res `shouldSatisfy` liftA2 (&&) (>=2) (<=8)
        it "evaluates sums" $
            eval (Sum (Constant 1) (Constant 7)) `shouldReturn` 8
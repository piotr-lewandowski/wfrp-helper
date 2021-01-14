module Test.ReadmeSpec (spec) where

import Control.Applicative
import Data.Either
import Model.Entity
import Model.Modifier
import Language.Dice
import Language.Main
import Lens.Simple hiding (Constant)
import Test.Hspec
import Printing

spec :: Spec
spec = parallel $  do
    describe "README examples" $ do
        it "dice evaluation" $ do
            res <- eval (fromRight (Constant 0) $ parse diceExpression "" "1d10 + 7")
            res `shouldSatisfy` liftA2 (&&) (>=2) (<=17)
        it "random stats" $ do
            res <- ($ archer human) <$> randomStats
            (res^.name) `shouldBe` "Human Archer"
        it "entity table" $
            printTable [human, elf, warrior dwarf, archer human] `shouldBe` unlines 
            [
                "| Name            | WS  | BS  | S   | T   | I   | Ag  | Dex | Int | WP  | Fel | W     |"
                , "---------------------------------------------------------------------------------------"
                , "| Human           |  30 |  30 |  30 |  30 |  30 |  30 |  30 |  30 |  30 |  30 |    12 |"
                , "| Elf             |  40 |  40 |  30 |  30 |  50 |  40 |  40 |  40 |  40 |  30 |    13 |"
                , "| Dwarf Warrior   |  50 |  30 |  40 |  50 |  30 |  20 |  40 |  30 |  50 |  20 |    16 |"
                , "| Human Archer    |  30 |  40 |  30 |  30 |  30 |  40 |  40 |  30 |  30 |  30 |    12 |"
            ]

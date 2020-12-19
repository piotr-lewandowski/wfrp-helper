module Test.EntitySpec (spec) where

import Data.Either
import Model.Entity
import Model.Modifier
import Model.Statline
import Language.Entity
import Language.Main
import Lens.Simple
import Test.Hspec

spec :: Spec
spec = parallel $  do
    describe "nameExpression parses entities" $ do
        it "human" $
            unsafeUnwrap (parse nameParser "" "human") `shouldSatisfy` (\ent -> ent^.statline == humanStatline && ent^.name == "Human")
        it "elf" $
            unsafeUnwrap (parse nameParser "" "elf") `shouldSatisfy` (\ent -> ent^.statline == elfStatline && ent^.name == "Elf")
        it "dwarf" $
            unsafeUnwrap (parse nameParser "" "dwarf") `shouldSatisfy` (\ent -> ent^.statline == dwarfStatline && ent^.name == "Dwarf")

    describe "modifierExpression parses modifiers" $ do
        it "archer" $
            parse modParser "" "archer" `shouldSatisfy` isRight
        it "warrior" $
            parse modParser "" "warrior" `shouldSatisfy` isRight
        it "captain" $
            parse modParser "" "captain" `shouldSatisfy` isRight

    describe "expression parser for entities" $ do            
        describe "parses entities with modifiers" $ do
            it "human archer" $
                unsafeUnwrap (parse entityExpression "" "human archer") `shouldSatisfy` 
                    (\ent -> ent^.statline == archer human ^. statline && ent^.name == "Human Archer")
            it "elf warrior captain" $
                unsafeUnwrap (parse entityExpression "" "elf warrior captain") `shouldSatisfy` 
                    (\ent -> ent^.statline == warrior (captain elf) ^. statline && ent^.name == "Elf Captain Warrior")

        describe "parses quantity expressions" $ do
            it "2d4 elf warrior" $
                parse entityExpression "" "2d4 elf warrior" `shouldSatisfy` (&&) <$> isRight <*> (isQuantityExpression . unsafeFromRight)
            it "1d3 dwarf warrior" $
                parse entityExpression "" "1d3 dwarf warrior" `shouldSatisfy` (&&) <$> isRight <*> (isQuantityExpression . unsafeFromRight)

        describe "parses groups" $ do
            it "human and 1d6 elf" $
                parse entityExpression "" "human and 1d6 elf" `shouldSatisfy` (&&) <$> isRight <*> (isAndExpression . unsafeFromRight)
            it "human and dwarf and elf" $
                parse entityExpression "" "human and dwarf and elf" `shouldSatisfy` (&&) <$> isRight <*> (isAndExpression . unsafeFromRight)
            it "2 human and elf archer" $
                parse entityExpression "" "2 human and elf archer" `shouldSatisfy` (&&) <$> isRight <*> (isAndExpression . unsafeFromRight)

isAndExpression :: EntityExpression -> Bool
isAndExpression (AndExpression _ _) = True
isAndExpression _ = False

isQuantityExpression :: EntityExpression -> Bool
isQuantityExpression (QuantityExpression _ _) = True
isQuantityExpression _ = False

unsafeFromRight :: Either a EntityExpression -> EntityExpression
unsafeFromRight (Right e) = e
unsafeFromRight _ = error "failed parsing"

unsafeUnwrap :: Either a EntityExpression -> Entity
unsafeUnwrap (Right (NameExpression ent)) = ent
unsafeUnwrap _ = error "failed parsing"

modParser :: Parser EntityExpression
modParser = choice $ map modifierExpression modifiers

nameParser :: Parser EntityExpression
nameParser = choice $ map nameExpression entities

-- Show instance to satisfy Hspec requirements
instance Show EntityExpression where
    show (NameExpression e) = e^.name
    show (ModifierExpression _ _) = "Modifier"
    show (AndExpression a b) = "And (" ++ show a ++ " " ++ show b ++ ")"
    show (QuantityExpression d e) = "Quantity (" ++ show d ++ " " ++ show e ++ ")"

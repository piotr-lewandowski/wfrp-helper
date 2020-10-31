module Language.Entity where

import Language.Dice
import Language.Main
import Model.Entity
import Model.Modifier

data EntityExpression = 
    NameExpression Entity |
    ModifierExpression Modifier |
    AndExpression EntityExpression EntityExpression |
    QuantityExpression DiceExpression EntityExpression

instance Show EntityExpression where
    show (NameExpression _) = "Name"
    show (ModifierExpression _) = "Modifier"
    show (AndExpression a b) = "And (" ++ show a ++ " " ++ show b ++ ")"
    show (QuantityExpression d e) = "Quantity (" ++ show d ++ " " ++ show e ++ ")"

entityExpression :: Parser EntityExpression
entityExpression = choice [andExpression, quantityExpression, singleEntity]

singleEntity :: Parser EntityExpression
singleEntity = try $ do
    mods <- modifierExpression `endBy` spaces
    (NameExpression ent) <- nameExpression
    let modifiers = map (\(ModifierExpression m) -> m) mods
    return $ NameExpression $ foldr ($) ent modifiers

modifierExpression :: Parser EntityExpression
modifierExpression = try $ do
    _ <- string "modifier"
    return $ ModifierExpression id

nameExpression :: Parser EntityExpression
nameExpression = try $ do
    n <- string "name"
    return $ NameExpression $ human { name = n }

andExpression :: Parser EntityExpression
andExpression = try $ do
    a <- choice [singleEntity, quantityExpression]
    _ <- spaces
    _ <- string "and"
    _ <- spaces
    b <- choice [andExpression, singleEntity, quantityExpression]
    return $ AndExpression a b

quantityExpression :: Parser EntityExpression
quantityExpression = try $ QuantityExpression <$> diceExpression <*> (spaces *> singleEntity)

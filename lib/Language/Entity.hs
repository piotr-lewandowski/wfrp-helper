module Language.Entity where

import Language.Dice
import Language.Main
import Model.Entity
import Model.Modifier
import Lens.Simple

data EntityExpression = 
    NameExpression Entity |
    ModifierExpression Modifier String |
    AndExpression EntityExpression EntityExpression |
    QuantityExpression DiceExpression EntityExpression

instance Show EntityExpression where
    show (NameExpression e) = e^.name
    show (ModifierExpression _ _) = "Modifier"
    show (AndExpression a b) = "And (" ++ show a ++ " " ++ show b ++ ")"
    show (QuantityExpression d e) = "Quantity (" ++ show d ++ " " ++ show e ++ ")"

modifiers :: [(String, Modifier)]
modifiers = [("warrior", warrior), ("archer", archer), ("captain", captain)]

entities :: [(String, Entity)]
entities = [("human", human), ("elf", elf), ("dwarf", dwarf)]

entityExpression :: Parser EntityExpression
entityExpression = choice [a, q, s]
    where
        a = andExpression s
        q = quantityExpression s
        s = singleEntity m n
        m = choice $ map modifierExpression modifiers
        n = choice $ map nameExpression entities

singleEntity :: Parser EntityExpression -> Parser EntityExpression -> Parser EntityExpression
singleEntity m n = try $ do
    modExpsPre <- m `endBy` spaces
    (NameExpression ent) <- n
    _ <- spaces
    modExpsPost <- m `endBy` spaces
    let mods = map (\(ModifierExpression x _) -> x) $ modExpsPre ++ modExpsPost
    return $ NameExpression $ foldr ($) ent mods

modifierExpression :: (String, Modifier) ->  Parser EntityExpression
modifierExpression (s, m) = try $ do
    _ <- string s
    return $ ModifierExpression m s

nameExpression :: (String, Entity) -> Parser EntityExpression
nameExpression (s, e) = try $ do
    _ <- string s
    return $ NameExpression e

andExpression :: Parser EntityExpression -> Parser EntityExpression
andExpression entityParser = try $ do
    a <- choice [entityParser, quantityExpression entityParser]
    _ <- spaces
    _ <- string "and"
    _ <- spaces
    b <- choice [andExpression entityParser, entityParser, quantityExpression entityParser]
    return $ AndExpression a b

quantityExpression :: Parser EntityExpression -> Parser EntityExpression
quantityExpression entity = try $ QuantityExpression <$> diceExpression <*> (spaces *> entity)

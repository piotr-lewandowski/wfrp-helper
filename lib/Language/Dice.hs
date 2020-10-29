module Language.Dice where

import Text.Parsec 
import Language.Main
import Model.Dice

data DiceExpression =
    Constant Int |
    Single Dice |
    Multiple ManyDice |
    Sum DiceExpression DiceExpression deriving (Show, Eq)

eval :: DiceExpression -> IO Int
eval (Constant n)   = return n 
eval (Single d)     = roll d
eval (Multiple md)  = roll md
eval (Sum a b)      = (+) <$> eval a <*> eval b

diceExpression :: Parser DiceExpression
diceExpression = choice [plus, multiple, single, constant]

constant :: Parser DiceExpression
constant = Constant <$> positiveInt

single :: Parser DiceExpression
single = Single <$> dice

dice :: Parser Dice
dice = try $ oneOf "kKdD" >> Dice <$> positiveInt

multiple :: Parser DiceExpression
multiple = Multiple <$> manyDice 

manyDice :: Parser ManyDice
manyDice = try $ ManyDice <$> positiveInt <*> dice

plus :: Parser DiceExpression
plus = try $ do
    a <- choice [multiple, single, constant]
    _ <- many space
    _ <- char '+'
    _ <- many space
    b <- choice [plus, multiple, single, constant]
    return $ Sum a b

positiveInt :: (Integral a, Read a) => Parser a
positiveInt = try $ do
    x <- oneOf "123456789"
    xs <- many digit
    return $ read (x:xs)

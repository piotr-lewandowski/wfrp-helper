module Dice where

import Text.ParserCombinators.Parsec
import Text.Parsec 
import Control.Monad

data Dice = Dice Int deriving Show
data ManyDice = ManyDice Int Dice deriving Show

data Expression =
    Constant Int |
    Single Dice |
    Multiple ManyDice |
    Sum Expression Expression deriving Show

class Roll a where
    roll :: a -> IO Int

instance Roll Dice where
    roll (Dice d) = undefined

instance Roll ManyDice where
    roll (ManyDice n d) = sum <$> (replicateM n $ roll d)

eval :: Expression -> IO Int
eval (Constant n)   = return n 
eval (Single d)     = roll d
eval (Multiple md)  = roll md
eval (Sum a b)      = (+) <$> (eval a) <*> (eval b)

expression :: Parser Expression
expression = choice [multiple, single, constant]

constant :: Parser Expression
constant = Constant <$> positiveInt

single :: Parser Expression
single = Single <$> dice

dice :: Parser Dice
dice = do
    oneOf "kKdD"
    i <- positiveInt 
    return $ Dice i

multiple :: Parser Expression
multiple = Multiple <$> manyDice 

manyDice :: Parser ManyDice
manyDice = do
    n <- positiveInt
    d <- dice
    return $ ManyDice n d

plus :: Parser Expression
plus = undefined

positiveInt :: (Integral a, Read a) => Parser a
positiveInt = do
    x <- oneOf "123456789"
    xs <- many digit
    return $ read (x:xs)

int :: (Integral a, Read a) => Parser a
int = read <$> many1 digit
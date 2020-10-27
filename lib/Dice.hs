module Dice where

import Text.Parsec 
import Control.Monad
import System.Random

data Dice = Dice Int deriving (Show, Eq)
data ManyDice = ManyDice Int Dice deriving (Show, Eq)

type Parser = Parsec String () 

data Expression =
    Constant Int |
    Single Dice |
    Multiple ManyDice |
    Sum Expression Expression deriving (Show, Eq)

class Roll a where
    roll :: a -> IO Int

instance Roll Dice where
    roll (Dice d) = getStdRandom (randomR (1,d))

instance Roll ManyDice where
    roll (ManyDice n d) = sum <$> (replicateM n $ roll d)

eval :: Expression -> IO Int
eval (Constant n)   = return n 
eval (Single d)     = roll d
eval (Multiple md)  = roll md
eval (Sum a b)      = (+) <$> (eval a) <*> (eval b)

expression :: Parser Expression
expression = choice [plus, multiple, single, constant] <* many space <* eof

constant :: Parser Expression
constant = Constant <$> positiveInt

single :: Parser Expression
single = Single <$> dice

dice :: Parser Dice
dice = try $ do
    oneOf "kKdD"
    i <- positiveInt 
    return $ Dice i

multiple :: Parser Expression
multiple = Multiple <$> manyDice 

manyDice :: Parser ManyDice
manyDice = try $ do
    n <- positiveInt
    d <- dice
    return $ ManyDice n d

plus :: Parser Expression
plus = try $ do
    a <- choice [multiple, single, constant]
    many space
    char '+'
    many space
    b <- choice [plus, multiple, single, constant]
    return $ Sum a b

positiveInt :: (Integral a, Read a) => Parser a
positiveInt = try $ do
    x <- oneOf "123456789"
    xs <- many digit
    return $ read (x:xs)

int :: (Integral a, Read a) => Parser a
int = read <$> many1 digit
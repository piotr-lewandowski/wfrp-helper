{-# LANGUAGE TemplateHaskell #-}
module Model.Statline where

import Lens.Simple

data Statline = Statline 
    {
        _weaponSkill :: Int,
        _ballisticSkill :: Int,
        _strength :: Int,
        _toughness :: Int,
        _initiative :: Int,
        _agility :: Int,
        _dexterity :: Int,
        _inteligence :: Int,
        _willpower :: Int,
        _fellowship :: Int
    } deriving (Show, Eq)

$(makeLenses ''Statline)

fromList :: [Int] -> Maybe Statline
fromList [a,b,c,d,e,f,g,h,i,j] = Just $ Statline a b c d e f g h i j
fromList _ = Nothing

toList :: Statline -> [Int]
toList (Statline a b c d e f g h i j) = [a,b,c,d,e,f,g,h,i,j]
 
humanStatline :: Statline
humanStatline = Statline 30 30 30 30 30 30 30 30 30 30

elfStatline :: Statline
elfStatline = Statline 40 40 30 30 50 40 40 40 40 30

dwarfStatline :: Statline
dwarfStatline = Statline 40 30 30 40 30 20 40 30 50 20

halflingStatline :: Statline
halflingStatline = Statline 20 40 20 30 30 30 40 30 40 40
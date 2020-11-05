{-# LANGUAGE TemplateHaskell #-}
module Model.Entity where

import Model.Item
import Model.Statline
import Lens.Simple

data Entity = Entity
    {
        _name :: String,
        _statline :: Statline,
        _wounds :: Statline -> Int,
        _equipment :: Equipment
    }

$(makeLenses ''Entity)

instance Show Entity where
    show (Entity n s w e) = "Entity " ++ show n ++ " " ++ show s ++ " " ++ show (w s) ++ " " ++ show e

human :: Entity
human = Entity "Human" humanStatline humanWounds []

elf :: Entity
elf = Entity "Elf" elfStatline humanWounds []

dwarf :: Entity
dwarf = Entity "Dwarf" dwarfStatline humanWounds []


humanWounds :: Statline -> Int
humanWounds stats = sb + 2*tb + wb 
    where 
        sb = getBonus _strength stats
        tb = getBonus _toughness stats
        wb = getBonus _willpower stats

getBonus :: (Statline -> Int) -> Statline -> Int
getBonus f s = f s `div` 10
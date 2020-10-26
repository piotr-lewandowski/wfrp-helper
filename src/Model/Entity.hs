module Model.Entity where

import Model.Item
import Model.Statline


data Entity = Entity
    {
        name :: String,
        statline :: Statline,
        wounds :: Statline -> Int,
        equipment :: Equipment
    }

human :: Entity
human = Entity "Human" humanStatline humanWounds []

humanWounds :: Statline -> Int
humanWounds stats = sb + 2*tb + wb 
    where 
        sb = getBonus _strength stats
        tb = getBonus _toughness stats
        wb = getBonus _willpower stats

getBonus :: (Statline -> Int) -> Statline -> Int
getBonus f s = f s `div` 10
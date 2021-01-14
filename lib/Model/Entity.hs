{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Model.Entity where

import Model.Item
import Model.Main
import Model.Statline
import Lens.Simple
import GHC.Generics
import Data.Aeson

data Entity = Entity
    {
        _name :: String,
        _statline :: Statline,
        _wounds :: Int,
        _equipment :: Equipment
    } deriving (Eq, Generic, Show)

instance ToJSON Entity where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

instance FromJSON Entity where
    parseJSON = genericParseJSON customOptions

$(makeLenses ''Entity)

human :: Entity
human = Entity "Human" humanStatline (humanWounds humanStatline) []

elf :: Entity
elf = Entity "Elf" elfStatline (humanWounds elfStatline) []

dwarf :: Entity
dwarf = Entity "Dwarf" dwarfStatline (humanWounds dwarfStatline) []

humanWounds :: Statline -> Int
humanWounds stats = sb + 2*tb + wb 
    where 
        sb = getBonus _strength stats
        tb = getBonus _toughness stats
        wb = getBonus _willpower stats

getBonus :: (Statline -> Int) -> Statline -> Int
getBonus f s = f s `quot` 10
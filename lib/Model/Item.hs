{-# LANGUAGE DeriveGeneric #-}
module Model.Item where

import Data.Aeson
import GHC.Generics
import Model.Main

data Item = 
    Weapon 
    { 
        name :: String,
        damage :: Int,
        qualities :: String
    } 
    | Armor { name :: String, protection :: Int }
    | Other { name :: String, description :: String }
    deriving (Eq, Generic, Show)

instance ToJSON Item where
    toJSON     = genericToJSON customOptions
    toEncoding = genericToEncoding customOptions

instance FromJSON Item where
    parseJSON = genericParseJSON customOptions

type Equipment = [Item]
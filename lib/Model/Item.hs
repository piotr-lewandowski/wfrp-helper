module Model.Item where


data Item = 
    Weapon 
    { 
        name :: String,
        damage :: Int,
        qualities :: String
    } 
    | Armor { name :: String, protection :: Int }
    | Other { name :: String, description :: String }

type Equipment = [Item]
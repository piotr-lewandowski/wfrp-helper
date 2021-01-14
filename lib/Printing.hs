module Printing where

import Model.Entity
import Model.Statline
import Text.Printf

printStats :: Statline -> String
printStats s = "| " ++ go (toList s)
    where
        go [] = ""
        go [x] = printf "%3d" x ++ " |"
        go (x:y:xs) = printf "%3d" x ++ " | " ++ go (y:xs)

printEntity :: Entity -> String
printEntity e@(Entity n s _ _) = "| " ++ printf nameFormat n ++ " " ++ printStats s ++ " " ++ printWounds e ++ " |"

printWounds :: Entity -> String
printWounds (Entity _ _ w _) = printf "%5d" w

nameFormat :: String
nameFormat = "%-15.15s"

printTable :: [Entity] -> String
printTable ents = unlines $ header : horizontal : map printEntity ents
    where 
        header = "| " ++ printf nameFormat "Name" ++ " | WS  | BS  | S   | T   | I   | Ag  | Dex | Int | WP  | Fel | W     |"
        len = length header
        horizontal = replicate len '-'

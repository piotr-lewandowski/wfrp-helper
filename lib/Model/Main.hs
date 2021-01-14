module Model.Main where

import Data.Aeson

customOptions :: Options
customOptions = defaultOptions
                { 
                    fieldLabelModifier = tail
                }

module Model.Dice where

import System.Random
import Control.Monad

newtype Dice = Dice Int deriving (Eq, Show)
data ManyDice = ManyDice Int Dice deriving (Eq, Show)

class Roll a where
    roll :: a -> IO Int

instance Roll Dice where
    roll (Dice d) = getStdRandom (randomR (1,d))

instance Roll ManyDice where
    roll (ManyDice n d) = sum <$> replicateM n (roll d)
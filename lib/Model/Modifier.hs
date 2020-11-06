module Model.Modifier where

import System.Random
import Model.Entity
import Model.Statline
import Control.Monad
import Lens.Simple

type Modifier = (Entity -> Entity)

randomStats :: IO Modifier
randomStats = do
    rolls <- replicateM 10 $ getStdRandom (randomR (2,20))
    let randomModifiers = map (subtract 10) rolls
        modifier e = e & statline .~ newStats
            where (Just newStats) = fromList $ zipWith (+) randomModifiers $ toList $ e^.statline
    return modifier

warrior :: Modifier
warrior (Entity n stats w eq) = Entity (n ++ " Warrior") newStats w eq
    where
        newStats = stats & weaponSkill +~ 10 & strength +~ 10 & toughness +~ 10

archer :: Modifier
archer (Entity n stats w eq) = Entity (n ++ " Archer") newStats w eq
    where
        newStats = stats & ballisticSkill +~ 10 & agility +~ 10 & dexterity +~ 10

captain :: Modifier
captain (Entity n stats w eq) = Entity (n ++ " Captain") newStats w eq
    where
        newStats = stats & ballisticSkill +~ 10 & willpower +~ 10 & fellowship +~ 10

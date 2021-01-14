module Data.People where

import Model.Statline
import Model.Entity

albrechtAnderssen :: Entity
albrechtAnderssen = Entity "Albrecht" stats (humanWounds stats) []
    where
        stats = Statline
            {
                _weaponSkill = 30,
                _ballisticSkill = 30,
                _strength = 30,
                _toughness = 30,
                _initiative = 30,
                _agility = 30,
                _dexterity = 30,
                _inteligence = 30,
                _willpower = 30,
                _fellowship = 30
            }
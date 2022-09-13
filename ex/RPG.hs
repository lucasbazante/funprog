{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}

module RPG where

-- Improvise!  A Character could have, for example:
-- a Name, a Race, a Class, a Level, XP (experience points),
-- 6 attributes:
--   Strength Intelligence Wisdom Dexterity Constitution Charisma
-- current and total HP (hit points)
-- current and total MP (mana points)
-- current and total GP (gold pieces)
data Character = Character { name :: String,
                             className :: String,
                             xp :: Int,
                             level :: Int,
                             intel :: Int,
                             dext :: Int,
                             str :: Int,
                             hp :: Int,
                             mp :: Int,
                             gp :: Int
                           } deriving ( Show )

-- does that make sense?
type Party = [Character]

-- gets a character and returns one that is the same but +1 level
gainLevel :: Character -> Character
gainLevel Character{..} = (Character name className xp (level + 1) intel dext str hp mp gp)

-- to be used when a character is hit
hitCharacter :: Character -> Int -> Character
hitCharacter Character{..} hit | alive Character{..} = (Character name className xp level intel dext str (hp - hit) mp gp)
                               | otherwise           = Character{..}

alive :: Character -> Bool
alive Character{..} = if hp > 0 then True else False 


-- How would you implement skills and spells?

data Skill = Slash | Crush | Disarm | Berserk | MakePotion | ReadGrimoire | RegenHP

data Spell = Fireball | Waterball | Thunder | Slow

skills :: Character -> [Skill]
skills Character{..} | dext >= 50 = [Slash, Disarm, RegenHP]
                     | dext >= 35 = [Disarm, RegenHP]
                     | str  >= 50 = [Crush, Berserk, RegenHP]
                     | str  >= 35 = [Crush, RegenHP]
                     | intel >= 50 = [MakePotion, ReadGrimoire, RegenHP]
                     | intel >= 35 = [MakePotion, RegenHP]
                     | otherwise  = [RegenHP]

spells :: Character -> [Spell]
spells Character{..} | intel >= 60 = [Thunder, Fireball, Slow, Waterball]
                     | intel >= 45 = [Fireball, Slow, Waterball]
                     | intel >= 25 = [Slow, Waterball]
                     | intel >= 15 = [Waterball]

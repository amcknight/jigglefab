module State.Struct
( Struct(..)
, wallStruct, orbStruct
, orbAt
, addOrb, removeOrb, replaceOrb
) where

import State.Wall
import State.Orb
import Geometry.Vector

data Struct c = Struct
  { structWalls :: [Wall]
  , orbs :: [Orb c]
  } deriving Show

instance Semigroup (Struct c) where
  (<>) :: Struct c -> Struct c -> Struct c
  (<>) (Struct ws1 os1) (Struct ws2 os2) = Struct (ws1 <> ws2) (os1 <> os2)
instance Monoid (Struct c) where
  mempty :: Struct c
  mempty = Struct [] []

wallStruct :: Wall -> Struct c
wallStruct w = Struct [w] []

orbStruct :: Orb c -> Struct c
orbStruct o = Struct [] [o]

orbAt :: Struct c -> Position -> Maybe (Orb c)
orbAt s p = case nearestOrb of
  Nothing -> Nothing
  Just nearOrb -> if distSq p (orbPos nearOrb) < 1
    then nearestOrb
    else Nothing
  where nearestOrb = minWith (distSq p . orbPos) (orbs s)

addOrb :: Orb c -> Struct c -> Struct c
addOrb o s = s {orbs = o : orbs s}

removeOrb :: Eq c => Orb c -> Struct c -> Struct c
removeOrb badO s = s {orbs = filter (/= badO) (orbs s)}

replaceOrb :: Eq c => Orb c -> Orb c -> Struct c -> Struct c
replaceOrb oldO newO s = s {orbs = fmap (\o -> if o == oldO then newO else o) (orbs s) }

minWith :: Ord b => (a -> b) -> [a] -> Maybe a
minWith _ [] = Nothing
minWith _ [x] = Just x
minWith f (x:xs) = if f x < f minTailX
  then Just x
  else Just minTailX
  where Just minTailX = minWith f xs

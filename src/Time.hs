module Time 
( Time
, Duration
, Speed
, Mover
, move
) where

type Time = Double
type Duration = Double
type Speed = Double

class Mover a where
  move :: Duration -> a -> a

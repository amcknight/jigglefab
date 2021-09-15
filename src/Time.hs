module Time 
( Time
, Duration
, Speed
, Mover
, move
) where

type Time = Float
type Duration = Float
type Speed = Float

class Mover a where
  move :: Duration -> a -> a

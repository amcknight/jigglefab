module Time 
( Time
, Duration
, Mover
, move
) where

type Time = Float
type Duration = Float

class Mover a where
  move :: Duration -> a -> a

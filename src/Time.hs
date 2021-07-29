module Time 
( Time
, Duration
, Interval
, moveInterval
) where

import Pair

type Time = Float
type Duration = Float
type Interval = (Time, Time)

moveInterval :: Duration -> Interval -> Interval
moveInterval dt = bimap (dt -)

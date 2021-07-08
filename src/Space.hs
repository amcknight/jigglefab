module Space
( Duration
, Radius
, Time
, minFuture
) where

type Time = Float
type Duration = Float
type Radius = Float

minFuture :: (Time, Time) -> Maybe Time
minFuture (t1, t2)
  | t1 < 0,  t2 < 0  = Nothing 
  | t1 >= 0, t2 < 0  = Just t1
  | t1 < 0,  t2 >= 0 = Just t2
  | t1 < t2          = Just t1
  | otherwise        = Just t2
module Space
( Radius
, Side (In, Out)
, flipSide
-- , Contact (Bounce, Pass)
-- , updateSide
) where

type Radius = Float

data Side = In | Out deriving (Show, Eq)
flipSide :: Side -> Side
flipSide In = Out
flipSide Out = In

data Contact = Bounce | Pass deriving Show

updateSide :: Side -> Contact -> Side
updateSide In  Bounce = In
updateSide In  Pass   = Out
updateSide Out Bounce = Out
updateSide Out Pass   = In

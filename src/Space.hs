module Space
( Radius
, Side (In, Out)
, Ortho (Vertical, Horizontal)
, flipSide
) where

type Radius = Float

data Ortho = Vertical | Horizontal deriving (Eq, Show)

data Side = In | Out deriving (Show, Eq)
flipSide :: Side -> Side
flipSide In = Out
flipSide Out = In

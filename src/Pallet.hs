module Pallet
( Pallet
, nicePallet
, getHot
, getWarm
, getNeutral
, getCool
, getCold
) where

import Graphics.Gloss

data RGB = RGB
  { r::Int
  , g::Int
  , b::Int
  }

data Pallet = Pallet
  { hot :: RGB
  , warm :: RGB
  , neutral :: RGB
  , cool :: RGB
  , cold ::RGB
  }

getHot = toColor . hot
getWarm = toColor . warm
getNeutral = toColor . neutral
getCool = toColor . cool
getCold = toColor . cold

nicePallet = Pallet
  (RGB 216 224 187)
  (RGB 182 206 199)
  (RGB 134 163 195)
  (RGB 114 104 166)
  (RGB 107  48 116)

toColor :: RGB -> Color 
toColor (RGB r g b) = makeColor (fromIntegral r/255) (fromIntegral g/255) (fromIntegral b/255) 1

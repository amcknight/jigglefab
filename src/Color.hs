module Color
( Color(..)
, toGlossColor
, mix
, black, white
, dark, light
, red, green, blue
, cyan, yellow, magenta
) where

import Data.Fixed (mod')
import Geometry.Angle
import qualified Graphics.Gloss.Data.Color as C

data RGB = RGB
  { r::Int
  , g::Int
  , b::Int
  }

data Color = Grey Float | Color
  { h::Angle
  , s::Float 
  , v::Float
  }

black :: Color
black = Grey 0
white :: Color
white = Grey 1

red :: Color
red = Color 0 1 1
green :: Color
green = Color (1/3) 1 1
blue :: Color
blue = Color (2/3) 1 1

cyan :: Color 
cyan = mix blue green
yellow :: Color 
yellow = mix green red
magenta :: Color 
magenta = mix red blue

light :: Color -> Color
light = mix white
dark :: Color -> Color
dark = mix black

mix :: Color -> Color -> Color
mix (Grey v1) (Grey v2) = Grey (avg v1 v2)
mix g@(Grey v) c = mix c g
mix c@(Color h s v1) (Grey v2) = mix c (Color h 0 v2)
mix (Color h1 s1 v1) (Color h2 s2 v2) = Color (mixHue h1 h2) (avg s1 s2) (avg v1 v2)

mixHue :: Angle -> Angle -> Angle
mixHue h1 h2 = case compare (abs (h1 - h2)) 0.5 of
  LT -> a
  EQ -> if h1 < h2 then a else pole a
  GT -> pole a
  where a = avg h1 h2

toRGB :: Color -> RGB
toRGB (Grey v) = RGB (toVal v) (toVal v) (toVal v)
toRGB (Color h s v) = RGB (toVal (r'+m)) (toVal (g'+m)) (toVal (b'+m))
  where
    c = v*s
    x = c * (1 - abs (((h*6) `mod'` 2) - 1))
    m = v-c
    (r',g',b')
      | h < 0 = undefined
      | h < 1/6 = (c,x,0)
      | h < 2/6 = (x,c,0)
      | h < 3/6 = (0,c,x)
      | h < 4/6 = (0,x,c)
      | h < 5/6 = (x,0,c)
      | h < 6/6 = (c,0,x)
      | otherwise = undefined

toVal :: Float -> Int
toVal = round . (255*)

fromVal :: Int -> Float
fromVal = (/255) . fromIntegral

rgbToColor :: RGB -> C.Color
rgbToColor (RGB r g b) = C.makeColor (fromVal r) (fromVal g) (fromVal b) 1

toGlossColor :: Color -> C.Color 
toGlossColor = rgbToColor . toRGB

avg :: Float -> Float -> Float 
avg a b = (a+b)/2

module Encode.Encode
( Encode (..)
, encodeModel
) where

import Chem
import Form
import Utils
import Model
import Geometry.Space
import Wall
import FormLibrary
import Color
import Geometry.Vector
import Point
import Time

data Sig = Red | Blue | Done deriving (Show, Eq, Ord)
data Active = Off | On Sig deriving (Show, Eq, Ord)
data Encode = Wire Active | Port Side Active | Actor Active deriving (Show, Eq, Ord)

instance Chem Encode where
  chemColor (Wire Off) = Grey 0.5
  chemColor (Wire (On Red)) = mix red $ Grey 0.5
  chemColor (Wire (On Blue)) = mix cyan $ Grey 0.5
  chemColor (Wire (On Done)) = mix magenta $ Grey 0.5
  chemColor (Port _ Off) = green
  chemColor (Port _ (On Red)) = mix red green
  chemColor (Port _ (On Blue)) = mix cyan green
  chemColor (Port _ (On Done)) = mix magenta green
  chemColor (Actor Off) = yellow
  chemColor (Actor (On Red)) = mix red yellow
  chemColor (Actor (On Blue)) = mix cyan yellow
  chemColor (Actor (On Done)) = mix magenta yellow

instance InnerChem Encode where
  innerReact (Wire Off, Wire a) = InExchange (Wire a, Wire Off)
  innerReact cs = InExchange cs
  allowThru sc = False

encodeLoopForm :: Radius -> Speed -> Int -> Position -> [Sig] -> R (Form Encode)
encodeLoopForm rad speed size pos sigs = do
  let [s1,s2,s3,s4] = sigs
  let stepFactor = 0.9
  let adjacentStep = rad*stepFactor
  let diagStep = sqrt ((adjacentStep^2)/2)
  let up = adjacentStep |* upV
  let down = adjacentStep |* downV
  let lp1 = pos |+ up
  let lp2 = lp1 |+ (diagStep |* upRightV)
  let lp3 = lp2 |+ up
  let lp4 = lp3 |+ up
  let lp5 = lp4 |+ up
  let lp6 = lp5 |+ (diagStep |* upLeftV)
  let lp7 = lp6 |+ (diagStep |* downLeftV)
  let lp8 = lp7 |+ down
  let lp9 = lp8 |+ down
  let lp10 = lp9 |+ down
  loop1 <- ballFormAt speed lp1 $ Actor Off
  loop2 <- ballFormAt speed lp2 $ Port Out Off
  loop3 <- ballFormAt speed lp3 $ Wire $ On Done
  loop4 <- ballFormAt speed lp4 $ Wire $ On s1
  loop5 <- ballFormAt speed lp5 $ Wire $ On s2
  loop6 <- ballFormAt speed lp6 $ Wire $ On s3
  loop7 <- ballFormAt speed lp7 $ Wire $ On s4
  loop8 <- ballFormAt speed lp8 $ Wire Off
  loop9 <- ballFormAt speed lp9 $ Wire Off
  loop10 <- ballFormAt speed lp10 $ Port In Off
  pure $ mconcat [loop1, loop2, loop3, loop4, loop5, loop6, loop7, loop8, loop9, loop10]

encodeModel :: R (Model Encode)
encodeModel = do
  let rad = 50
  let speed = rad*0.0002
  let slack = 3
  let boxSize = 1000
  let start = boxSize |* leftV
  let end = boxSize |* rightV
  let mid = 0.5 |* (start |+ end)
  prewire <- cappedLinChainFormExcl rad speed slack start mid [Wire (On Blue)] (Wire Off) []
  postwire <- linChainFormExcl rad speed slack mid end $ Wire Off
  encoderTip <- ballFormAt speed mid $ Actor Off
  let walls = wallForm (Circle start rad) <> wallForm (Circle end rad)
  let sigs = [Red, Blue, Blue, Red]
  encoder <- encodeLoopForm rad speed 5 mid sigs
  pure $ buildModel 50 $ walls <> prewire <> encoderTip <> encoder <> postwire

-- Split and Dup, Flip Sig, Ordered Join => Encode anything
-- Dup, Let and Flip -> Encode Anything
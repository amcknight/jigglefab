module Load.Load
( Load (..)
, loadModel
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

data Sig = Blue | Red deriving (Show, Eq, Ord)
data Active = Off | On Sig deriving (Show, Eq, Ord)
data Loader = Stem | StemWL | StemPort deriving (Show, Eq, Ord)
data Load = Wire Active | Port Side Active | Load Loader | Hold Load deriving (Show, Eq, Ord)

instance Chem Load where
  chemColor (Wire Off) = grey
  chemColor (Wire (On Red)) = mix red grey
  chemColor (Wire (On Blue)) = mix blue grey
  chemColor (Port _ Off) = mix green grey
  chemColor (Port _ (On Red)) = mix (mix red green) grey
  chemColor (Port _ (On Blue)) = mix (mix blue green) grey
  chemColor (Load _) = mix magenta grey
  chemColor (Hold c) = mix black $ chemColor c

instance InnerChem Load where
  innerReact (Wire Off, Wire a) = InExchange (Wire a, Wire Off)
  innerReact (Wire Off, Port Out (On s)) = InExchange (Wire (On s), Port Out Off)
  innerReact (Wire (On s), Port In Off) = InExchange (Wire Off, Port In (On s))
  innerReact (Wire (On Blue), Load Stem) = InExchange (Wire Off, Load StemWL)
  innerReact (Wire (On Blue), Load StemWL) = InBirth (Wire Off, Load Stem) (Hold (Wire Off))
  innerReact (Wire (On Blue), Load StemPort) = InBirth (Wire Off, Load Stem) (Port In Off)
  innerReact (Wire (On Red), Load Stem) = InExchange (Wire Off, Load StemPort)
  innerReact (Wire (On Red), Load StemWL) = InBirth (Wire Off, Load Stem) (Load Stem)
  innerReact (Wire (On Red), Load StemPort) = InBirth (Wire Off, Load Stem) (Port Out Off)
  innerReact (Port In (On Blue), Load Stem) = InExchange (Port In Off, Load StemWL)
  innerReact (Port In (On Blue), Load StemWL) = InBirth (Port In Off, Load Stem) (Hold (Wire Off))
  innerReact (Port In (On Blue), Load StemPort) = InBirth (Port In Off, Load Stem) (Hold (Port In Off))
  innerReact (Port In (On Red), Load Stem) = InExchange (Port In Off, Load StemPort)
  innerReact (Port In (On Red), Load StemWL) = InBirth (Port In Off, Load Stem) (Hold (Load Stem))
  innerReact (Port In (On Red), Load StemPort) = InBirth (Port In Off, Load Stem) (Hold (Port Out Off))
  innerReact cs = InExchange cs

  thruReact (Load s, Hold c) = (Load s, c)
  thruReact cs = cs

  allowThru ((Load _, Hold _), In) = True
  allowThru sc = False

loadModel :: R (Model Load)
loadModel = do
  let rad = 50
  let speed = rad*2
  let slack = 3
  let boxSize = 300
  let start = boxSize |* leftV
  let end = boxSize |* rightV
  let mid = 0.5 |* (start |+ end)

  let adjacentStep = 0.95*rad
  let diagStep = sqrt ((adjacentStep^2)/2)

  let walls = wallForm (Circle start rad) -- <> wallForm (Circle end rad)

  let sigs = [Red, Blue, Blue, Red]

  wire <- cappedLinChainFormExcl rad speed slack start end (fmap (Wire . On) sigs) (Wire Off) [Load Stem, Port In Off]
  -- ribo <- ballFormAt speed end $ Load Stem
  pure $ buildModel rad $ walls <> wire -- <> ribo

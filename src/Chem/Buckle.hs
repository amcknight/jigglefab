module Chem.Buckle
( Buckle (..)
, Sig (..)
, Active (..)
, turnbuckle
) where

import Chem
import Geometry.Space
import Color
import Geometry.Vector
import Wall
import StructLibrary
import Struct
import Orb

data Sig = Red | Blue deriving (Show, Eq, Ord)
data Active a = Off | On a deriving (Show, Eq, Ord)

data Buckle = Wire (Active Sig) | Port Side (Active Sig) | Flag Sig | Actor (Active Sig) | Sense Side deriving (Show, Eq, Ord)

-- a - O - O
-- Tighten a - O - O
-- Sense Destroyer a = Sensitize Apop O - O
-- Sense Destroyer a - Apop O = Sense O 
-- Destroyer a, Apop O, O
-- a - O

-- a - O
-- Loosen a - O
-- Drop Unflag a, Flag O, O
-- Unflag a - Flag O - O
-- a = O - O

instance Chem Buckle where
  chemColor (Wire Off) = grey
  chemColor (Wire (On Red)) = red
  chemColor (Wire (On Blue)) = cyan
  chemColor (Port Out Off) = dark yellow
  chemColor (Port Out (On Red)) = mix (dark yellow) red
  chemColor (Port Out (On Blue)) = mix (dark yellow) cyan
  chemColor (Port In Off) = dark green
  chemColor (Port In (On Red)) = mix (dark green) red
  chemColor (Port In (On Blue)) = mix (dark green) cyan
  chemColor (Flag Red) = dark red
  chemColor (Flag Blue) = dark blue
  chemColor (Actor Off) = magenta
  chemColor (Actor (On Red)) = mix red magenta
  chemColor (Actor (On Blue)) = mix blue magenta
  chemColor (Sense In) = black
  chemColor (Sense Out) = white

instance InnerChem Buckle where
  innerReact (Wire Off, Wire a) = InExchange (Wire a, Wire Off)
  innerReact (Wire Off, Actor (On Red)) = InExchange (Flag Red, Sense In)
  innerReact (Wire Off, Actor (On Blue)) = InBirth (Wire Off, Sense Out) (Flag Blue)
  innerReact (Wire Off, Port Out a) = InExchange (Wire a, Port Out Off)
  innerReact (Wire a, Port In Off) = InExchange (Wire Off, Port In a)
  innerReact (Port In a, Actor Off) = InExchange (Port In Off, Actor a)
  innerReact (Flag Red, Actor Off) = InRightOnly (Actor Off)
  innerReact (Flag Blue, Actor Off) = InExchange (Wire Off, Actor Off)
  innerReact cs = InExchange cs

  allowThru ((Wire Off, Sense In), Out) = True
  allowThru ((Wire Off, Sense Out), In) = True
  allowThru _ = False 

  thruReact (Wire Off, Sense _) = (Wire Off, Actor Off)
  thruReact c = c

turnbuckle :: Struct Buckle
turnbuckle = walls <> prechain <> buckle <> postchain
  where
    slack = 6
    boxSize = 20
    bottom = boxSize |* leftV
    top = boxSize |* rightV
    mid = zeroV
    sigs = fmap (Wire . On) [Red, Red, Red, Blue, Blue, Blue, Blue, Blue, Red, Red, Red]

    walls = mconcat $ fmap (\p -> wallStruct (rock p 1)) [bottom, top]
    prechain = cappedLinChainExcl slack bottom mid sigs (Wire Off) [Port In Off]
    buckle = orbStruct $ Orb mid $ Actor Off
    postchain = linChainExcl slack mid top $ Wire Off

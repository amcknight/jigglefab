module Chem.Sem
( Sem (..)
, Active (..)
, movingTool
) where

import State.Chem
import Color
import Geometry.Vector
import State.Wall
import State.StructLibrary
import State.Struct
import State.Orb
import GHC.Generics
import Util.Enumer
import Util.Side

data Sig = Red | Blue deriving (Show, Eq, Ord, Generic, Enumer)
data Active = Open | Closed | Full Sig deriving (Show, Eq, Ord, Generic, Enumer)
data Act = Sig Sig  -- Signal transfers freely to empty Wires
         | Apply -- Applies Actions (Take, Drop, Die, Spawn)
         | Done  -- Used by Port to move from Closed -> Open
         | Wait  -- Used by Port to block back-signals when Full
         | Take  -- Join Applier with Taker
         | Drop  -- Separate Applier and Dropper
         | Die   -- Applier kills the Dier
         | Spawn -- Applier and Spawner create an empty Wire
         | Hold  -- Does nothing until reacted with an empty Wire
         | Send [Act] -- Puts actions on empty Wire
         deriving (Show, Eq, Ord, Generic)
data Sem = Wire [Act] | Port Active deriving (Show, Eq, Ord, Generic)

instance Chem Sem where
  chemColor (Wire []) = grey
  chemColor (Wire ((Sig Red):_)) = mix red (light grey)
  chemColor (Wire ((Sig Blue):_)) = mix cyan (light grey)
  chemColor (Wire (Apply:_)) = yellow
  chemColor (Wire (Done:_)) = mix yellow red
  chemColor (Wire (Wait:_)) = dark grey
  chemColor (Wire (Take:_)) = white
  chemColor (Wire (Drop:_)) = white
  chemColor (Wire (Die:_)) = black
  chemColor (Wire (Spawn:_)) = green
  chemColor (Wire (Hold:_)) = dark grey
  chemColor (Wire ((Send _):_)) = red
  chemColor (Port Open) = magenta
  chemColor (Port Closed) = dark magenta
  chemColor (Port (Full Red)) = mix (light magenta) red
  chemColor (Port (Full Blue)) = mix (light magenta) cyan

instance InnerChem Sem where
  innerReact (Wire [], Wire ((Sig s):as)) = InExchange (Wire [Sig s], Wire as)
  innerReact (Wire [], Wire (Hold:as)) = InExchange (Wire [], Wire as)
  innerReact (Wire [], Wire ((Send send):as)) = InExchange (Wire send, Wire as)
  innerReact (Wire (Apply:as), Wire (Die:_)) = InLeftOnly (Wire as) -- Kinda weird to Die with more commands underneath
  innerReact (Wire (Apply:as1), Wire (Spawn:as2)) = InBirth (Wire as1, Wire as2) (Wire [])
  innerReact (Wire [], Port (Full Red)) = InExchange (Wire [Send [Send [Take], Hold, Die], Apply, Apply, Done], Port Closed) -- AUTO-load
  innerReact (Wire [], Port (Full Blue)) = InExchange (Wire [Send [Spawn, Drop], Apply, Apply, Done], Port Closed)  -- AUTO-load
  innerReact (Wire ((Sig s):as), Port Open) = InExchange (Wire (Wait:as), Port (Full s))
  innerReact (Wire (Done:as), Port Closed) = InExchange (Wire as, Port Open)
  innerReact (Wire (Wait:as), Port Closed) = InExchange (Wire as, Port Closed)
  innerReact cs = InExchange cs

  allowThru ((Wire (Apply:_), Wire (Take:_)), Out) = True
  allowThru ((Wire (Apply:_), Wire (Drop:_)), In) = True
  allowThru _ = False

  thruReact (Wire (Apply:as1), Wire (Take:as2)) = (Wire as1, Wire as2)
  thruReact (Wire (Apply:as1), Wire (Drop:as2)) = (Wire as1, Wire as2)
  thruReact c = c

movingTool :: Struct Sem
movingTool = walls <> chains <> buckles <> tool <> gate
  where
    slack = 3
    boxSize = 13.333
    mid = boxSize |* upV
    bottom = boxSize |* downV
    left = boxSize |* upLeftV
    right = boxSize |* upRightV
    midLeft = 0.5 |* (left |+ mid)
    midRight = 0.5 |* (right |+ mid)
    midBottom = 0.5 |* (bottom |+ mid)
    sigs = fmap (\s -> Wire [Sig s]) [Blue, Red, Red, Red, Red, Blue, Blue, Blue, Blue, Blue, Blue, Blue]
    walls = mconcat $ fmap (\p -> wallStruct (rock p 1)) [left, right, bottom]
    leftPrechain = linChainExcl slack left midLeft $ Wire []
    leftPostchain = linChainExcl slack midLeft mid $ Wire []
    rightPrechain = linChainExcl slack right midRight $ Wire []
    rightPostchain = linChainExcl slack midRight mid $ Wire []
    bottomPrechain = cappedLinChainExcl slack bottom midBottom sigs (Wire []) [Wire []]
    leftBottomPostChain = arcChainExcl 0.25 slack left midBottom $ Wire []
    rightBottomPostChain = arcChainExcl 0.25 slack midBottom right $ Wire []
    chains = leftPrechain <> leftPostchain <> rightPrechain <> rightPostchain <> bottomPrechain <> leftBottomPostChain <> rightBottomPostChain
    leftBuckle = orbStruct $ Orb midLeft $ Port Open
    rightBuckle = orbStruct $ Orb midRight $ Port Open
    buckles = leftBuckle <> rightBuckle
    tool = orbStruct $ Orb mid $ Wire [Wait]
    gate = orbStruct $ Orb midBottom $ Wire []

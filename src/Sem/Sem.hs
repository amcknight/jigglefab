module Sem.Sem
( Sem (..)
, Active (..)
, movingToolModel
) where

import Chem
import Geometry.Space
import Color
import Model
import Utils
import Geometry.Vector
import Form
import Wall
import FormLibrary

data Sig = Red | Blue deriving (Show, Eq, Ord)
data Active = Open | Closed | Full Sig deriving (Show, Eq, Ord)
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
         deriving (Show, Eq, Ord)
data Sem = Wire [Act] | Port Active deriving (Show, Eq, Ord)

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

sigNotForm :: R (Form Sem)
sigNotForm = undefined

-- sigSplitForm :: R (Form Sem)
-- sigSplitForm = do
--   pure $ 

movingToolModel :: R (Model Sem)
movingToolModel = do
  let rad = 60
  let speed = rad*3
  let slack = 3
  let boxSize = 800
  let numSigs = 1
  let mid = boxSize |* upV
  let bottom = boxSize |* downV
  let left = boxSize |* upLeftV
  let right = boxSize |* upRightV
  let midLeft = 0.5 |* (left |+ mid)
  let midRight = 0.5 |* (right |+ mid)
  let midBottom = 0.5 |* (bottom |+ mid)
  let sigs = fmap (\s -> Wire [Sig s]) [Blue, Red, Red, Red, Red, Blue, Blue, Blue, Blue, Blue, Blue, Blue]
  let walls = mconcat $ fmap (\p -> wallForm (Circle p rad)) [left, right, bottom]
  leftPrechain <- linChainFormExcl rad speed slack left midLeft $ Wire []
  leftPostchain <- linChainFormExcl rad speed slack midLeft mid $ Wire []
  rightPrechain <- linChainFormExcl rad speed slack right midRight $ Wire []
  rightPostchain <- linChainFormExcl rad speed slack midRight mid $ Wire []
  bottomPrechain <- cappedLinChainFormExcl rad speed slack bottom midBottom sigs (Wire []) [Wire []]
  leftBottomPostChain <- arcChainFormExcl rad speed 0.25 slack left midBottom $ Wire []
  rightBottomPostChain <- arcChainFormExcl rad speed 0.25 slack midBottom right $ Wire []
  let chains = leftPrechain <> leftPostchain <> rightPrechain <> rightPostchain <> bottomPrechain <> leftBottomPostChain <> rightBottomPostChain
  leftBuckle <- ballFormAt speed midLeft $ Port Open
  rightBuckle <- ballFormAt speed midRight $ Port Open
  let buckles = leftBuckle <> rightBuckle
  tool <- ballFormAt speed mid $ Wire [Wait]
  gate <- ballFormAt speed midBottom $ Wire []
  pure $ buildModel rad $ walls <> chains <> buckles <> tool <> gate

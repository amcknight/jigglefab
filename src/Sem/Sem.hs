module Sem.Sem
( Sem (..)
, Active (..)
, turnbuckleCrazyModel
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

data Active = Open | Full | Closed deriving (Show, Eq, Ord)
data Act = Sig   -- Signal transfers freely to empty Wires
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
  chemColor (Wire (Sig:_)) = light grey
  chemColor (Wire (Apply:_)) = blue
  chemColor (Wire (Hold:_)) = dark grey
  chemColor (Wire (Wait:_)) = dark grey
  chemColor (Wire _) = red
  chemColor (Port Open) = magenta
  chemColor (Port Full) = light magenta
  chemColor (Port Closed) = dark magenta

instance InnerChem Sem where
  innerReact (Wire [], Wire (Sig:as)) = InExchange (Wire [Sig], Wire as)
  innerReact (Wire [], Wire (Hold:as)) = InExchange (Wire [], Wire as)
  innerReact (Wire [], Wire ((Send send):as)) = InExchange (Wire send, Wire as)
  innerReact (Wire (Apply:as), Wire (Die:_)) = InLeftOnly (Wire as) -- Kinda weird to Die with more commands underneath
  innerReact (Wire (Apply:as1), Wire (Spawn:as2)) = InBirth (Wire as1, Wire as2) (Wire [])
  -- innerReact (Wire [], Port Full) = InExchange (Wire [Send [Send [Take], Hold, Die], Apply, Apply, Done], Port Closed) -- AUTO-encode
  innerReact (Wire [], Port Full) = InExchange (Wire [Send [Spawn, Drop], Apply, Apply, Done], Port Closed)  -- AUTO-encode
  innerReact (Wire (Sig:as), Port Open) = InExchange (Wire (Wait:as), Port Full)
  innerReact (Wire (Done:as), Port Closed) = InExchange (Wire as, Port Open)
  innerReact (Wire (Wait:as), Port Closed) = InExchange (Wire as, Port Closed)
  innerReact cs = InExchange cs

  allowThru ((Wire (Apply:_), Wire (Take:_)), Out) = True
  allowThru ((Wire (Apply:_), Wire (Drop:_)), In) = True
  allowThru _ = False 

  thruReact (Wire (Apply:as1), Wire (Take:as2)) = (Wire as1, Wire as2)
  thruReact (Wire (Apply:as1), Wire (Drop:as2)) = (Wire as1, Wire as2)
  thruReact c = c

turnbuckleCrazyModel :: R (Model Sem)
turnbuckleCrazyModel = do
  let rad = 100
  let speed = rad*2
  let slack = 4
  let boxSize = 500
  let bottom = boxSize |* leftV
  let top = boxSize |* rightV
  let mid = zeroV
  let sigs = fmap Wire (replicate 5 [Sig])
  let walls = mconcat $ fmap (\p -> wallForm (Circle p rad)) [bottom, top]
  prechain <- cappedLinChainFormExcl rad speed slack bottom mid sigs (Wire []) [Port Open]
  postchain <- linChainFormExcl rad speed slack mid top $ Wire []
  buckle <- ballFormAt speed mid $ Wire []
  pure $ buildModel rad $ walls <> prechain <> buckle <> postchain

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

data Active = Off | On deriving (Show, Eq, Ord)

data Do = Take | Drop | Die | Spawn deriving (Show, Eq, Ord)
data Command = Hold Command | Act Do | DoThen Do Command | Send Command Command deriving (Show, Eq, Ord)
data Sem = Wire Active | Port Side Active | Actor (Maybe Command) | Command Command deriving (Show, Eq, Ord)

-- a - O - O
-- a (Send (Act Take) (Hold (Act Die))) - O - O
-- a - C (Send (Act Take) (Hold (Act Die))) - O
-- a - C (Hold (Act Die)) - C (Act Take)
-- a, C (Hold (Act Die)), O
-- a, C (Act Die), O
-- a - O

-- a - O
-- a (C (DoThen Spawn (Act Drop))) - O
-- a - C (DoThen Spawn (Act Drop))
-- a, O, C (Act Drop)
-- a - O - O

instance Chem Sem where
  chemColor (Wire Off) = grey
  chemColor (Wire On) = cyan
  chemColor (Port Out Off) = magenta
  chemColor (Port Out On) = mix cyan magenta
  chemColor (Port In Off) = green
  chemColor (Port In On) = mix cyan green
  chemColor (Actor Nothing) = dark red
  chemColor (Actor (Just _)) = red
  chemColor (Command (Hold _)) = white 
  chemColor (Command _) = black 

instance InnerChem Sem where
  innerReact (Wire Off, Wire On) = InExchange (Wire On, Wire Off)
  innerReact (Wire Off, Port Out On) = InExchange (Wire On, Port Out Off)
  innerReact (Wire Off, Command (Hold c)) = InExchange (Wire Off, Command c)
  innerReact (Wire Off, Command (Send send keep)) = InExchange (Command send, Command keep)
  innerReact (Wire Off, Actor (Just c)) = InExchange (Command c, Actor Nothing)
  innerReact (Wire On, Port In Off) = InExchange (Wire Off, Port In On)
  innerReact (Port In On, Actor Nothing) = InExchange (Port In Off, Actor (Just (Send (Act Take) (Hold (Act Die))))) -- AUTO-encode
  innerReact (Actor Nothing, Command (Act Die)) = InLeftOnly (Actor Nothing)
  innerReact (Actor Nothing, Command (Act Spawn)) = InBirth (Actor Nothing, Wire Off) (Wire Off)
  innerReact (Actor Nothing, Command (DoThen Die c)) = InLeftOnly (Actor Nothing) -- Kinda weird to Die with more commands underneath
  innerReact (Actor Nothing, Command (DoThen Spawn c)) = InBirth (Actor Nothing, Command c) (Wire Off)
  innerReact cs = InExchange cs

  allowThru ((Actor Nothing, Command (Act Take)), Out) = True
  allowThru ((Actor Nothing, Command (Act Drop)), In) = True
  allowThru ((Actor Nothing, Command (DoThen Take _)), Out) = True
  allowThru ((Actor Nothing, Command (DoThen Drop _)), In) = True
  allowThru _ = False 

  thruReact (Actor Nothing, Command (Act Take)) = (Actor Nothing, Wire Off)
  thruReact (Actor Nothing, Command (Act Drop)) = (Actor Nothing, Wire Off)
  thruReact (Actor Nothing, Command (DoThen Take c)) = (Actor Nothing, Command c)
  thruReact (Actor Nothing, Command (DoThen Drop c)) = (Actor Nothing, Command c)
  thruReact c = c

turnbuckleCrazyModel :: R (Model Sem)
turnbuckleCrazyModel = do
  let rad = 50
  let speed = rad*4
  let slack = 6
  let boxSize = 1000
  let bottom = boxSize |* leftV
  let top = boxSize |* rightV
  let mid = zeroV
  let sigs = fmap Wire (replicate 1 On)
  let walls = mconcat $ fmap (\p -> wallForm (Circle p rad)) [bottom, top]
  prechain <- cappedLinChainFormExcl rad speed slack bottom mid sigs (Wire Off) [Port In Off]
  postchain <- linChainFormExcl rad speed slack mid top $ Wire Off
  buckle <- ballFormAt speed mid $ Actor Nothing
  pure $ buildModel rad $ walls <> prechain <> buckle <> postchain

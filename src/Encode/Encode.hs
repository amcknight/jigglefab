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

data Sig = Red | Blue deriving (Show, Eq, Ord)
data Active = Off | On Sig deriving (Show, Eq, Ord)
data Sync = Open | Hold Sig | Emit Sig deriving (Show, Eq, Ord)
data Dup = Ready | Once Sig | Twice Sig deriving (Show, Eq, Ord)
data Encode = Wire Active | Port Side Active | Eat | Sync Sync | Dup Dup deriving (Show, Eq, Ord)

instance Chem Encode where
  chemColor (Wire Off) = Grey 0.5
  chemColor (Wire (On Red)) = mix red $ Grey 0.5
  chemColor (Wire (On Blue)) = mix cyan $ Grey 0.5
  chemColor (Port _ Off) = green
  chemColor (Port _ (On Red)) = mix red green
  chemColor (Port _ (On Blue)) = mix cyan green
  chemColor Eat = black 
  chemColor (Sync Open) = blue
  chemColor (Sync (Hold Red)) = mix red blue
  chemColor (Sync (Hold Blue)) = mix cyan blue
  chemColor (Sync (Emit Red)) = mix white $ mix red blue
  chemColor (Sync (Emit Blue)) = mix white $ mix cyan blue
  chemColor (Dup Ready) = yellow
  chemColor (Dup (Twice Red)) = mix red $ mix red yellow
  chemColor (Dup (Once Red)) = mix red yellow
  chemColor (Dup (Twice Blue)) = mix cyan $ mix cyan yellow
  chemColor (Dup (Once Blue)) = mix cyan yellow

instance InnerChem Encode where
  innerReact (Wire Off, Wire a) = InExchange (Wire a, Wire Off)
  innerReact (Wire Off, Port Out (On s)) = InExchange (Wire (On s), Port Out Off)
  innerReact (Wire Off, Sync (Emit s)) = InExchange (Wire (On s), Sync Open)
  innerReact (Wire Off, Dup (Once s)) = InExchange (Wire (On s), Dup Ready)
  innerReact (Wire Off, Dup (Twice s)) = InExchange (Wire (On s), Dup (Once s))
  innerReact (Wire (On s), Port In Off) = InExchange (Wire Off, Port In (On s))
  innerReact (Wire (On s), Eat) = InExchange (Wire Off, Eat)
  innerReact (Wire (On s), Sync Open) = InExchange (Wire Off, Sync (Hold s))
  innerReact (Wire (On s), Dup Ready) = InExchange (Wire Off, Dup (Twice s))
  innerReact (Port Out Off, Sync (Emit s)) = InExchange (Port Out (On s), Sync Open)
  innerReact (Port Out Off, Dup (Once s)) = InExchange (Port Out (On s), Dup Ready)
  innerReact (Port Out Off, Dup (Twice s)) = InExchange (Port Out (On s), Dup (Once s))
  innerReact (Port In (On s), Eat) = InExchange (Port In Off, Eat)
  innerReact (Port In (On s), Sync Open) = InExchange (Port In Off, Sync (Hold s))
  innerReact (Port In (On s), Dup Ready) = InExchange (Port In Off, Dup (Twice s))
  innerReact (Sync (Hold s1), Sync (Hold s2)) = InExchange (Sync (Emit s1), Sync (Emit s2))
  innerReact cs = InExchange cs
  allowThru sc = False

loopForm :: Radius -> Speed -> Int -> Position -> [Sig] -> R (Form Encode)
loopForm rad speed size pos sigs = do
  -- TODO this is all terribly hardcoded
  let [s1,s2,s3,s4] = sigs
  let stepFactor = 0.9
  let adjacentStep = rad*stepFactor
  let diagStep = sqrt ((adjacentStep^2)/2)
  let up = adjacentStep |* upV
  let down = adjacentStep |* downV
  let left = adjacentStep |* leftV
  let lp1 = pos |+ up
  let lp2 = lp1 |+ (diagStep |* upLeftV)
  let lp3 = lp2 |+ (diagStep |* upLeftV)
  let lp4 = lp3 |+ (diagStep |* upLeftV)
  let lp5 = lp4 |+ left
  let lp6 = lp5 |+ left
  let lp7 = lp6 |+ left
  let lp8 = lp7 |+ left
  let lp9 = lp8 |+ left
  let lp10 = lp9 |+ (diagStep |* downLeftV)
  let lp11 = lp10 |+ (diagStep |* downLeftV)
  let lp12 = lp11 |+ (diagStep |* downLeftV)
  let lp13 = lp12 |+ down
  -- let lp14 = lp13 |+ (diagStep |* downLeftV)
  loop1 <- ballFormAt speed lp1 $ Wire $ On s1
  loop2 <- ballFormAt speed lp2 $ Wire $ On s2
  loop3 <- ballFormAt speed lp3 $ Wire $ On s3
  loop4 <- ballFormAt speed lp4 $ Wire $ On s4
  loop5 <- ballFormAt speed lp5 $ Wire Off
  loop6 <- ballFormAt speed lp6 $ Wire Off
  loop7 <- ballFormAt speed lp7 $ Wire Off
  loop8 <- ballFormAt speed lp8 $ Wire Off
  loop9 <- ballFormAt speed lp9 $ Wire Off
  loop10 <- ballFormAt speed lp10 $ Wire Off
  loop11 <- ballFormAt speed lp11 $ Wire Off
  loop12 <- ballFormAt speed lp12 $ Wire Off
  loop13 <- ballFormAt speed lp13 $ Wire Off
  -- loop14 <- ballFormAt speed lp14 $ Wire Off
  pure $ mconcat [loop1, loop2, loop3, loop4, loop5, loop6, loop7, loop8, loop9, loop10, loop11, loop12, loop13]

andForm :: Radius -> Speed -> Position -> Sig -> R (Form Encode)
andForm rad speed pos sig = do
  let adjacentStep = 0.95*rad
  let up = adjacentStep |* upV
  let right = adjacentStep |* rightV
  botInP <- ballFormAt speed pos $ Port In Off
  topInP <- ballFormAt speed (pos |+ (2 |* up)) $ Port In Off
  botOutP <- ballFormAt speed (pos |+ (1.5 |* right)) $ Port Out Off
  topOutP <- ballFormAt speed (pos |+ (2 |* up) |+ (1.5 |* right)) $ Port Out Off
  let ports = [botInP, topInP, botOutP, topOutP]
  botSync <- ballFormAt speed (pos |+ (0.5 |* up) |+ (0.75 |* right)) $ Sync Open
  topSync <- ballFormAt speed (pos |+ (1.5 |* up) |+ (0.75 |* right)) $ Sync Open
  let syncs = [botSync, topSync]
  garbage <- ballFormAt speed (pos |+ (2.25 |* right)) $ Wire Off
  eat <- ballFormAt speed (pos |+ (3 |* right)) Eat
  pure $ mconcat $ ports ++ syncs ++ [garbage, eat]

splitForm :: Radius -> Speed -> Position -> R (Form Encode)
splitForm rad speed pos = do
  let adjacentStep = 0.95*rad
  let diagStep = sqrt ((adjacentStep^2)/2)
  let right = adjacentStep |* rightV
  let up = adjacentStep |* upV
  let down = adjacentStep |* downV
  let diagUp = diagStep |* upRightV
  let diagDown = diagStep |* downRightV
  inP <- ballFormAt speed pos $ Port In Off
  dup <- ballFormAt speed (pos |+ right) $ Dup Ready
  outP <- ballFormAt speed (pos |+ (2|*right)) $ Port Out Off
  bridge <- ballFormAt speed (pos |+ (3|*right)) $ Wire Off
  rightInP <- ballFormAt speed (pos |+ (4|*right)) (Port In Off)
  topS <- ballFormAt speed (pos |+ (4|*right) |+ diagUp |+ (0.2 |* down)) (Sync Open)
  botS <- ballFormAt speed (pos |+ (4|*right) |+ diagDown |+ (0.2 |* up)) (Sync Open)
  topOutP <- ballFormAt speed (pos |+ (5|*right) |+ diagUp) (Port Out Off)
  botOutP <- ballFormAt speed (pos |+ (5|*right) |+ diagDown) (Port Out Off)
  pure $ mconcat [inP, dup, outP, bridge, rightInP, topS, botS, topOutP, botOutP]

dupKForm :: Radius -> Speed -> Position -> Int -> R (Form Encode)
dupKForm rad speed pos num = do
  let adjacentStep = 0.95*rad
  let right = adjacentStep |* rightV

  pure $ mconcat []

dupSeqForm :: Radius -> Speed -> Position -> Int -> R (Form Encode)
dupSeqForm _ _ _ 0 = do pure mempty
dupSeqForm rad speed pos num = do
  let step = 0.95*rad
  let right = step |* rightV
  inP <-    ballFormAt speed (pos |+ (0|*right)) $ Port In Off
  dup <-    ballFormAt speed (pos |+ (1|*right)) $ Dup Ready
  outP <-   ballFormAt speed (pos |+ (2|*right)) $ Port Out Off
  bridge <- ballFormAt speed (pos |+ (3|*right)) $ Wire Off
  tail <- dupSeqForm rad speed (pos |+ (4|*right)) (num-1)
  pure $ mconcat [inP, dup, outP, bridge, tail]

encodeModel :: R (Model Encode)
encodeModel = do
  let rad = 50
  let speed = rad*5
  let slack = 3
  let boxSize = 1000
  let start = boxSize |* leftV
  let end = boxSize |* rightV
  let mid = 0.5 |* (start |+ end)
  let midLeft = 0.5 |* (start |+ mid)

  let adjacentStep = 0.95*rad
  let diagStep = sqrt ((adjacentStep^2)/2)
  let up = adjacentStep |* upV
  let right = adjacentStep |* rightV
  let diagUp = diagStep |* upRightV
  let diagDown = diagStep |* downRightV

  prewire <- cappedLinChainFormExcl rad speed slack start midLeft (replicate 3 (Wire (On Blue))) (Wire Off) []
  dup4 <- dupSeqForm rad speed midLeft 2
  postdupWire <- linChainFormExcl rad speed 1 (mid |- (4 |* right)) mid $ Wire Off
  blueAnd <- andForm rad speed mid Blue
  encoderTip <- ballFormAt speed mid $ Wire Off
  let walls = wallForm (Circle start rad) <> wallForm (Circle end rad)
  let sigs = [Red, Blue, Blue, Red]
  let blueAndOutPos = mid |+ ((2*adjacentStep) |* upV) |+ ((1.5*adjacentStep) |* rightV)
  bridge <- ballFormAt speed (blueAndOutPos |+ (adjacentStep |* rightV)) $ Wire Off
  let bridgeOutPos = blueAndOutPos |+ ((2*adjacentStep) |* rightV)
  split <- splitForm rad speed bridgeOutPos
  let splitOutTopPos = bridgeOutPos |+ (5|*right) |+ diagUp
  let splitOutBotPos = bridgeOutPos |+ (5|*right) |+ diagDown
  loop <- loopForm rad speed 5 splitOutTopPos sigs
  postwire <- linChainFormExcl rad speed slack splitOutBotPos end $ Wire Off
  pure $ buildModel rad $ walls <> prewire <> dup4 <> postdupWire <> blueAnd <> bridge <> split <> loop <> postwire

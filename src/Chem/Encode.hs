{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Chem.Encode
( Encode (..)
, encoder
) where

import Model.Chem
import Model.Wall
import StructLibrary
import Color
import Geometry.Vector
import Model.Struct
import Model.Orb
import GHC.Generics
import Util.Enumer
import Util.Side

data Sig = Red | Blue deriving (Show, Eq, Ord, Generic, Enumer)
data Active = Off | On Sig deriving (Show, Eq, Ord, Generic, Enumer)
data Sync = Open | Hold Sig | Emit Sig deriving (Show, Eq, Ord, Generic, Enumer)
data Dup = Ready | Once Sig | Twice Sig deriving (Show, Eq, Ord, Generic, Enumer)
data Encode = Wire Active | Port Side Active | Eat | Sync Sync | Dup Dup deriving (Show, Eq, Ord, Generic, Enumer)

instance Chem Encode where
  chemColor (Wire Off) = grey
  chemColor (Wire (On Red)) = mix red grey
  chemColor (Wire (On Blue)) = mix cyan grey
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
  innerReact (Wire Off,       Wire a) =          InExchange (Wire a,          Wire Off)
  innerReact (Wire Off,       Port Out (On s)) = InExchange (Wire (On s),     Port Out Off)
  innerReact (Wire Off,       Sync (Emit s)) =   InExchange (Wire (On s),     Sync Open)
  innerReact (Wire Off,       Dup (Once s)) =    InExchange (Wire (On s),     Dup Ready)
  innerReact (Wire Off,       Dup (Twice s)) =   InExchange (Wire (On s),     Dup (Once s))
  innerReact (Wire (On s),    Port In Off) =     InExchange (Wire Off,        Port In (On s))
  innerReact (Wire (On _),    Eat) =             InExchange (Wire Off,        Eat)
  innerReact (Wire (On s),    Sync Open) =       InExchange (Wire Off,        Sync (Hold s))
  innerReact (Wire (On s),    Dup Ready) =       InExchange (Wire Off,        Dup (Twice s))
  innerReact (Port Out Off,   Sync (Emit s)) =   InExchange (Port Out (On s), Sync Open)
  innerReact (Port Out Off,   Dup (Once s)) =    InExchange (Port Out (On s), Dup Ready)
  innerReact (Port Out Off,   Dup (Twice s)) =   InExchange (Port Out (On s), Dup (Once s))
  innerReact (Port In (On _), Eat) =             InExchange (Port In Off,     Eat)
  innerReact (Port In (On s), Sync Open) =       InExchange (Port In Off,     Sync (Hold s))
  innerReact (Port In (On s), Dup Ready) =       InExchange (Port In Off,     Dup (Twice s))
  innerReact (Sync (Hold s1), Sync (Hold s2)) =  InExchange (Sync (Emit s1),  Sync (Emit s2))
  innerReact cs = InExchange cs
  allowThru _ = False

-- TODO this is all terribly hardcoded
loopStruct :: Int -> Position -> [Active] -> Struct Encode
loopStruct _ pos sigs = mconcat loops
  where
    adjacentStep = 0.9
    diagStep = sqrt ((adjacentStep * adjacentStep)/2)
    up = adjacentStep |* upV
    down = adjacentStep |* downV
    left = adjacentStep |* leftV
    lp1 = pos |+ up
    lp2 = lp1 |+ (diagStep |* upLeftV)
    lp3 = lp2 |+ (diagStep |* upLeftV)
    lp4 = lp3 |+ (diagStep |* upLeftV)
    lp5 = lp4 |+ left
    lp6 = lp5 |+ left
    lp7 = lp6 |+ left
    lp8 = lp7 |+ left
    lp9 = lp8 |+ left
    lp10 = lp9 |+ (diagStep |* downLeftV)
    lp11 = lp10 |+ (diagStep |* downLeftV)
    lp12 = lp11 |+ (diagStep |* downLeftV)
    lp13 = lp12 |+ down
    lps = [lp1, lp2, lp3, lp4, lp5, lp6, lp7, lp8, lp9, lp10, lp11, lp12, lp13]
    actives = sigs ++ replicate (13 - length sigs) Off
    loops = fmap (\(pos, a) -> orbStruct (Orb pos (Wire a))) (zip lps actives)

andStruct :: Position -> Struct Encode
andStruct pos = mconcat $ ports ++ syncs ++ [garbage, eat]
  where
    adjacentStep = 0.95
    up = adjacentStep |* upV
    right = adjacentStep |* rightV
    botInP = orbStruct $ Orb pos $ Port In Off
    topInP = orbStruct $ Orb  (pos |+ (2 |* up)) $ Port In Off
    botOutP = orbStruct $ Orb (pos |+ (1.5 |* right)) $ Port Out Off
    topOutP = orbStruct $ Orb (pos |+ (2 |* up) |+ (1.5 |* right)) $ Port Out Off
    ports = [botInP, topInP, botOutP, topOutP]
    botSync = orbStruct $ Orb (pos |+ (0.5 |* up) |+ (0.75 |* right)) $ Sync Open
    topSync = orbStruct $ Orb (pos |+ (1.5 |* up) |+ (0.75 |* right)) $ Sync Open
    syncs = [botSync, topSync]
    garbage = orbStruct $ Orb (pos |+ (2.25 |* right)) $ Wire Off
    eat = orbStruct $ Orb (pos |+ (3 |* right)) Eat

splitStruct :: Position -> Struct Encode
splitStruct pos = mconcat [inP, dup, outP, bridge, rightInP, topS, botS, topOutP, botOutP]
  where
    adjacentStep = 0.95
    diagStep = sqrt ((adjacentStep*adjacentStep)/2)
    right = adjacentStep |* rightV
    up = adjacentStep |* upV
    down = adjacentStep |* downV
    diagUp = diagStep |* upRightV
    diagDown = diagStep |* downRightV
    inP = orbStruct $ Orb pos $ Port In Off
    dup = orbStruct $ Orb (pos |+ right) $ Dup Ready
    outP = orbStruct $ Orb (pos |+ (2|*right)) $ Port Out Off
    bridge = orbStruct $ Orb (pos |+ (3|*right)) $ Wire Off
    rightInP = orbStruct $ Orb (pos |+ (4|*right)) (Port In Off)
    topS = orbStruct $ Orb (pos |+ (4|*right) |+ diagUp |+ (0.2 |* down)) (Sync Open)
    botS = orbStruct $ Orb (pos |+ (4|*right) |+ diagDown |+ (0.2 |* up)) (Sync Open)
    topOutP = orbStruct $ Orb (pos |+ (5|*right) |+ diagUp) (Port Out Off)
    botOutP = orbStruct $ Orb (pos |+ (5|*right) |+ diagDown) (Port Out Off)

dupSeqStruct :: Position -> Int -> Struct Encode
dupSeqStruct _ 0 = mempty
dupSeqStruct pos num = mconcat [inP, dup, outP, bridge, tail]
  where
    step = 0.95
    right = step |* rightV
    inP =    orbStruct $ Orb (pos |+ (0|*right)) $ Port In Off
    dup =    orbStruct $ Orb (pos |+ (1|*right)) $ Dup Ready
    outP =   orbStruct $ Orb (pos |+ (2|*right)) $ Port Out Off
    bridge = orbStruct $ Orb (pos |+ (3|*right)) $ Wire Off
    tail = dupSeqStruct (pos |+ (4|*right)) (num-1)

encoder :: Struct Encode
encoder = walls <> prewire <> dup4 <> postdupWire <> blueAnd <> bridge <> split <> loop <> postwire
  where
    slack = 3
    boxSize = 20
    start = boxSize |* leftV
    end = boxSize |* rightV
    mid = 0.5 |* (start |+ end)
    midLeft = 0.5 |* (start |+ mid)

    adjacentStep = 0.95
    diagStep = sqrt ((adjacentStep*adjacentStep)/2)
    right = adjacentStep |* rightV
    diagUp = diagStep |* upRightV
    diagDown = diagStep |* downRightV

    prewire = cappedLinChainExcl slack start midLeft (replicate 3 (Wire (On Blue))) (Wire Off) []
    dup4 = dupSeqStruct midLeft 2
    postdupWire = linChainExcl 1 (mid |- (4 |* right)) mid $ Wire Off
    blueAnd = andStruct mid
    walls = wallStruct (rock start 1) <> wallStruct (rock end 1)
    sigs = [Red, Blue, Blue, Red]
    blueAndOutPos = mid |+ ((2*adjacentStep) |* upV) |+ ((1.5*adjacentStep) |* rightV)
    bridge = orbStruct $ Orb (blueAndOutPos |+ (adjacentStep |* rightV)) $ Wire Off
    bridgeOutPos = blueAndOutPos |+ ((2*adjacentStep) |* rightV)
    split = splitStruct bridgeOutPos
    splitOutTopPos = bridgeOutPos |+ (5|*right) |+ diagUp
    splitOutBotPos = bridgeOutPos |+ (5|*right) |+ diagDown
    loop = loopStruct 5 splitOutTopPos $ fmap On sigs
    postwire = linChainExcl slack splitOutBotPos end $ Wire Off

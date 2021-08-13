module Model
( Model (rad, form)
, buildModel
, step
, innerIps
) where

import Data.Maybe (mapMaybe)
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Vector as V
import Space
import Time
import Pair
import Point
import Ball
import Hit
import Wall
import Vector
import Form
import Chem
import Debug.Trace

type SideMap = M.Map (P Int) Side
data Model c = Model
  { rad :: Radius
  , form :: Form c
  , wbSides :: SideMap
  , bbSides :: SideMap
  , bounces :: [Hit]
  } deriving Show

instance Mover (Model c) where
  move dt (Model r f wss hss hs) = Model r (move dt f) wss hss (fmap (move dt) hs)

buildModel :: Chem c => Radius -> Form c -> Model c
buildModel rad f = tieAll $ populateHits $ Model rad f wss hss []
  where
    wss = M.fromList $ wbSide f <$> bonkIndices f
    hss = M.fromList $ bbSide rad f <$> bounceIndices f
    
    populateHits :: Model c -> Model c
    populateHits (Model r f wss hss _) = Model r f wss hss $ L.sort $ hitsFromIps r f $ bounceIndices f

    tieAll :: Chem c => Model c -> Model c
    tieAll m = ties (innerIps m) m

    ties :: Chem c => [P Int] -> Model c -> Model c
    ties [] m = m
    ties (ip:ips) m = ties ips $ tie1 ip m

    tie1 :: Chem c => P Int -> Model c -> Model c
    tie1 ip m = replacePair m ip In $ buildBalls (bi point bs) (prereact (bi chem bs, In))
      where
        bs = bi (ballByI (form m)) ip

innerIps :: Model c -> [P Int]
innerIps m = innerIps' $ M.assocs $ bbSides m
  where
    innerIps' :: [Sided Int] -> [P Int]
    innerIps' [] = []
    innerIps' ((ip, Out):ss) = innerIps' ss
    innerIps' ((ip, In):ss) = ip : innerIps' ss

bbSideByI :: Model c -> P Int -> Side
bbSideByI m = (bbSides m M.!)

wbSideByI :: Model c -> P Int -> Side
wbSideByI m = (wbSides m M.!)

replace :: Model c -> Int -> Ball c -> Model c
replace (Model r oldF wss hss oldHs) i b = Model r newF wss hss $ updateBounces1 r newF i oldHs
  where newF = replaceBall i b oldF

replacePair :: Model c -> P Int -> Side -> P (Ball c) -> Model c
replacePair (Model r oldF wbs bbs oldHs) bbi s bs = Model r newF wbs newBbs (updateBounces2 r newF bbi oldHs)
  where
    newBbs = M.insert bbi s bbs
    newF = replaceBalls bbi bs oldF

step :: Chem c => Duration -> Model c -> Model c
step dt m = case (nextBonk m, nextBounce m) of
  (Nothing, Nothing) -> move dt m
  (Nothing, Just (Hit bt s ip)) -> case compare dt bt of
    LT -> move dt m
    _ -> step (dt - bt) $ bounceModel s ip $ move bt m
  (Just (Hit bt s ip), Nothing) -> case compare dt bt of
    LT -> move dt m
    _ -> step (dt - bt) $ bonkModel s ip $ move bt m
  (Just bk, Just bc) ->
    let Hit t s ip = minimum [bk, bc]
        newDt = dt - t
    in case compare dt t of
      LT -> move dt m
      _ -> case compare bk bc of
        LT -> step newDt $ bonkModel s ip $ move t m
        _ -> step newDt $ bounceModel s ip $ move t m

nextBonk :: Model c -> Maybe Hit
nextBonk m = nextValidBonk m bonks
  where
    bonks = L.sort (mapMaybe (toModelBonk m) (bonkIndices f))
    Model rad f _ _ _ = m
    nextValidBonk :: Model c -> [Hit] -> Maybe Hit
    nextValidBonk _ [] = Nothing 
    nextValidBonk m (b:bs) = if wbSideByI m ip == s then Just b else nextValidBonk m bs
      where Hit _ s ip = b
      
nextBounce :: Model c -> Maybe Hit
nextBounce m = nextValidBounce m $ bounces m
  where
    nextValidBounce :: Model c -> [Hit] -> Maybe Hit
    nextValidBounce _ [] = Nothing 
    nextValidBounce m (h:hs) = if bbSideByI m ip == s then Just h else nextValidBounce m hs
      where (Hit _ s ip) = h

updateBounces1 :: Radius -> Form c -> Int -> [Hit] -> [Hit]
updateBounces1 r f i hs = L.sort $ keep ++ newHits
  where
    keep = filter (uneffected i) hs
    newHits = hitsFromIps r f $ pairsOfTo1 (length (balls f)) i

    uneffected :: Int -> Hit -> Bool
    uneffected i h = not $ (overlaps1 i . ixPair) h

updateBounces2 :: Radius -> Form c -> P Int -> [Hit] -> [Hit]
updateBounces2 r f ip hs = L.sort $ keep ++ newHits
  where
    keep = filter (uneffected ip) hs
    newHits = hitsFromIps r f $ pairsOfTo2 (length (balls f)) ip

    uneffected :: P Int -> Hit -> Bool
    uneffected ip h = not $ (overlaps2 ip . ixPair) h

hitsFromIps :: Radius -> Form c -> [P Int] -> [Hit]
hitsFromIps r f = concatMap (times r f)
  where
    times :: Radius -> Form c -> P Int -> [Hit]
    times r f ip = fmap (toHit ip) (hitTimes r (bi (point . ballByI f) ip))
    toHit :: P Int -> (Time, Side) -> Hit
    toHit ip (t, s) = Hit t s ip

bounceModel :: Chem c => Side -> P Int -> Model c -> Model c
bounceModel s ip m = replacePair m ip newS $ buildBalls newPs newCs
  where
    bs = (bi . ballByI) (form m) ip
    ps = bi point bs
    cs = bi chem bs
    (newCs, newS) = react (cs, s)
    newPs = if s == newS then bounce ps else ps

bonkModel :: Side -> P Int -> Model c -> Model c
bonkModel s (wi, li) m = replace m li newBall
  where
    f = form m
    Wall o _ = wallByI f wi
    Ball p c = ballByI f li
    newBall = Ball (bonk o p) c

toModelBonk :: Model c -> P Int -> Maybe Hit
toModelBonk m ip = toBonk (form m) (wbSideByI m ip) ip

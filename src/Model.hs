module Model
( Model (Model, rad, form, wSides, hSides)
, buildModel
, wSideByI, hSideByI
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

type SideMap = M.Map (P Int) Side
data Model c = Model
  { rad :: Radius
  , form :: Form c
  , wSides :: SideMap
  , hSides :: SideMap
  , hits :: [Hit]
  } deriving Show

instance Mover (Model c) where
  move dt (Model r f wss hss hs) = Model r (move dt f) wss hss (fmap (move dt) hs)

buildModel :: Chem c => Radius -> Form c -> Model c
buildModel rad f = tieAll $ populateHits $ Model rad f wss hss []
  where
    wss = M.fromList $ findWSides f <$> bonkIndices f
    hss = M.fromList $ findHSides rad f <$> bounceIndices f

    findWSides :: Form c -> P Int -> Sided Int
    findWSides f (wi, bi) = case o of
      Vertical -> case compare pl x of
        LT -> ((wi, bi), Out)
        _ -> ((wi, bi), In)
      Horizontal -> case compare pl y of
        LT -> ((wi, bi), Out)
        _ -> ((wi, bi), In)
      where
        (x, y) = coords (pos p)
        Wall o pl = wallByI f wi
        Ball p _ = ballByI f bi

    findHSides :: Radius -> Form c -> P Int -> Sided Int
    findHSides rad f ip = (ip, hSide)
      where hSide = side rad $ bi (point . ballByI f) ip
    
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
        bs = (bi . ballByI) (form m) ip

innerIps :: Model c -> [P Int]
innerIps m = innerIps' $ M.assocs $ hSides m
  where
    innerIps' :: [Sided Int] -> [P Int]
    innerIps' [] = []
    innerIps' ((ip, Out):ss) = innerIps' ss
    innerIps' ((ip, In):ss) = ip : innerIps' ss

hSideByI :: Model c -> P Int -> Side
hSideByI m = (hSides m M.!)

wSideByI :: Model c -> P Int -> Side
wSideByI m = (wSides m M.!)

replace :: Model c -> Int -> Ball c -> Model c
replace (Model r oldF wss hss oldHs) i b = Model r newF wss hss $ updateBounces1 r newF i oldHs
  where newF = replaceBall i b oldF

replacePair :: Model c -> P Int -> Side -> P (Ball c) -> Model c
replacePair (Model r oldF wss hss oldHs) ip s bs = Model r newF wss hss $ updateBounces2 r newF ip oldHs
  where newF = replaceBalls ip bs oldF

step :: Chem c => Duration -> Model c -> Model c
step dt m = case (nextBonk m, nextBounce m) of
  (Nothing, Nothing) -> move dt m
  (Nothing, Just (Hit bt s ip)) ->
    if dt < bt then move dt m
    else step (dt - bt) $ bounceModel s ip $ move bt m
  (Just (Hit bt s wlip), Nothing) ->
    if dt < bt then move dt m
    else step (dt - bt) $ bonkModel s wlip $ move bt m
  (Just (Hit bnkTime bs wlip), Just (Hit bncTime hs ip)) -> case compare bnkTime bncTime of
    LT ->
      if dt < bnkTime then move dt m
      else step (dt - bnkTime) $ bonkModel bs wlip $ move bnkTime m
    GT ->
      if dt < bncTime then move dt m
      else step (dt - bncTime) $ bounceModel hs ip $ move bncTime m
    EQ ->
      if dt < bncTime then move dt m
      else step (dt - bncTime) $ bounceModel hs ip $ bonkModel bs wlip $ move bncTime m

nextBonk :: Model c -> Maybe Hit
nextBonk m = nextValidBonk m bonks
  where
    bonks = L.sort (mapMaybe (toModelBonk m) (bonkIndices f))
    Model rad f _ _ _ = m
    nextValidBonk :: Model c -> [Hit] -> Maybe Hit
    nextValidBonk _ [] = Nothing 
    nextValidBonk m (b:bs) = if wSideByI m ip == s then Just b else nextValidBonk m bs
      where Hit _ s ip = b
      
nextBounce :: Model c -> Maybe Hit
nextBounce m = nextValidBounce m $ hits m
  where
    nextValidBounce :: Model c -> [Hit] -> Maybe Hit
    nextValidBounce _ [] = Nothing 
    nextValidBounce m (h:hs) = if hSideByI m ip == s then Just h else nextValidBounce m hs
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
    times r f ip = hitTimes r (bi point ((bi . ballByI) f ip)) ip

hitTimes :: Radius -> P Point -> P Int -> [Hit]
hitTimes rad ps ip = case root of
  Nothing -> []
  Just r -> times ip $ possTimes r
  where
    times :: P Int -> (Time, Time) -> [Hit]
    times ip (t1, t2)
      | highT < 0 = []
      | lowT < 0 = [Hit highT In ip]
      | otherwise = [Hit lowT Out ip, Hit highT In ip]
      where
        (lowT, highT) = if t1 < t2 then (t1, t2) else (t2, t1)
    
    possTimes :: Float -> (Time, Time)
    possTimes r = ((r - s)/speedSq, (-r - s)/speedSq)

    safeRoot :: Float -> Maybe Float
    safeRoot x
      | x < 0     = Nothing
      | otherwise = Just $ sqrt x

    root = safeRoot $ speedSq * (rad^2 - lengthSq (pos diff)) + s^2
    s = pos diff |. vel diff
    speedSq = lengthSq $ vel diff
    diff = minus ps

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
toModelBonk m ip = toBonk (form m) (wSideByI m ip) ip

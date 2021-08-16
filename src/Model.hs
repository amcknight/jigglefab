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
import Data.Bifunctor

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
    tie1 ip m = replacePair m ip In $ buildBalls (pmap point bs) (prereact (pmap chem bs, In))
      where
        bs = pmap (ballByI (form m)) ip

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

remove :: Model c -> Int -> Model c
remove (Model r f wbs bbs hs) bi = Model r (removeBall f bi) newWbs newBbs newHs
  where
    newWbs = M.mapKeys (second decIfOver) $ M.filterWithKey (\ (_,i) _ -> bi /= i) wbs
    newBbs = M.mapKeys (\ (i,j) -> (decIfOver i, decIfOver j)) $ M.filterWithKey (\ (i,j) _ -> bi /= i && bi /= j) bbs
    newHs = (\(Hit dt s (i,j)) -> Hit dt s (decIfOver i, decIfOver j)) <$> filter (\(Hit dt s (i,j)) -> bi /= i && bi /= j) hs
    
    decIfOver :: Int -> Int
    decIfOver i = if i > bi then i-1 else i

add :: Model c -> Ball c -> Model c
add (Model r f@(Form ws bs) wbs bbs hs) b = Model r newF newWbs newBbs newHs
  where
    i = length bs
    wis = [0..length ws - 1]
    bis = [0..length bs - 1] -- Doesn't include new ball index i
    wallsWithI = zip wis $ V.toList ws
    ballsWithI = zip bis $ V.toList bs

    newF = addBall f b
    newWbs = M.union wbs $ M.fromList $ fmap (\ (wi, w) -> ((wi, i), wSide w (pos (point b)))) wallsWithI
    newBbs = M.union bbs $ M.fromList $ fmap (\ (bi, b2) -> (sortP (bi, i), side r (point b, point b2))) ballsWithI
    newHs = L.sort $ hs ++ hitsFromIps r newF (fmap (\bi -> sortP (bi, i)) bis)

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
    bonks = L.sort (mapMaybe (toModelBonk m) (bonkIndices (form m)))
    nextValidBonk :: Model c -> [Hit] -> Maybe Hit
    nextValidBonk _ [] = Nothing 
    nextValidBonk m (b@(Hit _ s ip):bs) = if wbSideByI m ip == s then Just b else nextValidBonk m bs
      
nextBounce :: Model c -> Maybe Hit
nextBounce m = nextValidBounce m $ bounces m
  where
    nextValidBounce :: Model c -> [Hit] -> Maybe Hit
    nextValidBounce _ [] = Nothing 
    nextValidBounce m (b@(Hit _ s ip):bs) = if bbSideByI m ip == s then Just b else nextValidBounce m bs

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
    times r f ip = fmap (toHit ip) (hitTimes r (pmap (point . ballByI f) ip))
    toHit :: P Int -> (Time, Side) -> Hit
    toHit ip (t, s) = Hit t s ip

bounceModel :: Chem c => Side -> P Int -> Model c -> Model c
bounceModel s ip@(i1,i2) m = case react (cs, s) of
  LeftOnly newC -> 
    let (v1, v2) = pmap vel ps
        newP = Point (pos p1) (v1 |+ v2)
    in remove (replace m i1 (Ball newP newC)) i2
  RightOnly newC ->
    let (v1, v2) = pmap vel ps
        newP = Point (pos p2) (v1 |+ v2)
    in remove (replace m i2 (Ball newP newC)) i1
  Exchange (newCs, newS) ->
    let newPs = if s == newS then bounce ps else ps
    in replacePair m ip newS $ buildBalls newPs newCs
  Birth (newCs, newS) newC ->
    let newPs = if s == newS then bounce ps else ps
    in add (replacePair m ip newS (buildBalls newPs newCs)) (Ball (birthPoint p1 p2) newC)
  where
    bs = pmap (ballByI (form m)) ip
    ps@(p1, p2) = pmap point bs
    cs = pmap chem bs

bonkModel :: Side -> P Int -> Model c -> Model c
bonkModel s (wi, li) m = replace m li newBall
  where
    f = form m
    Wall o _ = wallByI f wi
    Ball p c = ballByI f li
    newBall = Ball (bonk o p) c

toModelBonk :: Model c -> P Int -> Maybe Hit
toModelBonk m ip = toBonk (form m) (wbSideByI m ip) ip

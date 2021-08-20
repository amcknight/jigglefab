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
import HitTime

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
buildModel rad f = prereactAll $ populateHits $ Model rad f wss hss []
  where
    wss = M.fromList $ wbSide f <$> bonkIndices f
    hss = M.fromList $ bbSide rad f <$> bounceIndices f
    
    populateHits :: Model c -> Model c
    populateHits (Model r f wss hss _) = Model r f wss hss $ L.sort $ hitsFromIps r f $ bounceIndices f

    prereactAll :: Chem c => Model c -> Model c
    prereactAll m = foldr prereactPair m (innerIps m)

    prereactPair :: Chem c => P Int -> Model c -> Model c
    prereactPair ip m = replacePair ip In newBs m
      where
        newBs = buildBalls (pmap point bs) (prereact (pmap chem bs, In))
        bs = pmap (ballI (form m)) ip

innerIps :: Model c -> [P Int]
innerIps = fmap fst . filter isIn . M.assocs . bbSides

bbSideI :: Model c -> P Int -> Side
bbSideI m = (bbSides m M.!)

wbSideI :: Model c -> P Int -> Side
wbSideI m = (wbSides m M.!)

replace :: Int -> Ball c -> Model c -> Model c
replace i b (Model r oldF wss hss oldHs) = Model r newF wss hss $ updateBounces1 r newF i oldHs
  where newF = replaceBall i b oldF

replacePair :: P Int -> Side -> P (Ball c) -> Model c -> Model c
replacePair bbi s bs (Model r oldF wbs bbs oldHs) = Model r newF wbs newBbs (updateBounces2 r newF bbi oldHs)
  where
    newBbs = M.insert bbi s bbs
    newF = replaceBalls bbi bs oldF

remove :: Int -> Model c -> Model c
remove bi (Model r f wbs bbs hs) = Model r (removeBall f bi) newWbs newBbs newHs
  where
    newWbs = M.mapKeys (second decIfOver) $ M.filterWithKey (\ (_,i) _ -> bi /= i) wbs
    newBbs = M.mapKeys (pmap decIfOver) $ M.filterWithKey (\ (i,j) _ -> bi /= i && bi /= j) bbs
    newHs = (\(Hit dt s (i,j)) -> Hit dt s (decIfOver i, decIfOver j)) <$> filter (\(Hit dt s (i,j)) -> bi /= i && bi /= j) hs
    
    decIfOver :: Int -> Int
    decIfOver i = if i > bi then i-1 else i

add :: Ball c -> Model c -> Model c
add b (Model r f@(Form ws bs) wbs bbs hs) = Model r newF newWbs newBbs newHs
  where
    i = length bs
    wis = [0..length ws - 1]
    bis = [0..length bs - 1] -- Doesn't include new ball index i
    wallsWithI = zip wis $ V.toList ws
    ballsWithI = zip bis $ V.toList bs

    newF = addBall f b
    newWbs = M.union wbs $ M.fromList $ fmap (\(wi, w) -> ((wi, i), wSide w (pos (point b)))) wallsWithI
    newBbs = M.union bbs $ M.fromList $ fmap (\(bi, b2) -> (sortP (bi, i), side r (point b, point b2))) ballsWithI
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
    f = form m
    bonks = L.sort $ mapMaybe (toBonkFromModel m) (bonkIndices f)
    nextValidBonk :: Model c -> [Hit] -> Maybe Hit
    nextValidBonk _ [] = Nothing 
    nextValidBonk m (b@(Hit _ s ip):bs) = if wbSideI m ip == s then Just b else nextValidBonk m bs
    toBonkFromModel :: Model c -> P Int -> Maybe Hit
    toBonkFromModel m ip = toBonk f (wbSideI m ip) ip
      
nextBounce :: Model c -> Maybe Hit
nextBounce m = nextValidBounce m $ bounces m
  where
    nextValidBounce :: Model c -> [Hit] -> Maybe Hit
    nextValidBounce _ [] = Nothing 
    nextValidBounce m (b@(Hit _ s ip):bs) = if bbSideI m ip == s then Just b else nextValidBounce m bs

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
    times r f ip = fmap (toHit ip) (toList (hitTimes r (pmap (point . ballI f) ip)))
    toHit :: P Int -> (Time, Side) -> Hit
    toHit ip (t, s) = Hit t s ip

bounceModel :: Chem c => Side -> P Int -> Model c -> Model c
bounceModel s ip@(i1,i2) m = case react (cs, s) of
  LeftOnly newC -> 
    let (v1, v2) = pmap vel ps
        newP = Point (pos p1) (v1 |+ v2)
    in remove i2 (replace i1 (Ball newP newC) m)
  RightOnly newC ->
    let (v1, v2) = pmap vel ps
        newP = Point (pos p2) (v1 |+ v2)
    in remove i1 (replace i2 (Ball newP newC) m)
  Exchange (newCs, newS) ->
    let newPs = if s == newS then bounce ps else ps
    in replacePair ip newS (buildBalls newPs newCs) m
  Birth (newCs, newS) newC ->
    let newPs = if s == newS then bounce ps else ps
    in add (Ball (birthPoint p1 p2) newC) (replacePair ip newS (buildBalls newPs newCs) m)
  where
    bs = pmap (ballI (form m)) ip
    ps@(p1, p2) = pmap point bs
    cs = pmap chem bs

bonkModel :: Side -> P Int -> Model c -> Model c
bonkModel s (wi, li) m = replace li newBall m
  where
    f = form m
    w = wallI f wi
    Ball p c = ballI f li
    newBall = case w of
      VLine _ -> Ball (bonkVLine p) c
      HLine _ -> Ball (bonkHLine p) c
      Circle pl r -> Ball (bonkCircle pl r p) c

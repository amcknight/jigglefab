module Model
( Model (Model, rad, time, sides, links)
, buildModel
, sideByI, linksByI
, step
, nextHit
, moveModel, bounceModel
, hits
, innerIps
, updateHits
) where

import Data.Maybe (mapMaybe)
import qualified Data.Array as A
import qualified Data.Map as M
import qualified Data.List as L
import Space
import Time
import Pair
import Points
import Chems
import Link
import Links
import Hit

type LinkArray = A.Array Int Link
type SideMap = M.Map (Int, Int) Side
data Model = Model
  { rad :: Radius
  , time :: Time
  , hits :: [Hit]
  , sides :: SideMap
  , links :: LinkArray
  } deriving Show

-- Assumes all initial Chem's have: "has" == 0
buildModel :: Radius -> [Link] -> Model
buildModel r ls = tieAll $ populateHits $ Model r 0 [] sideMap lsArray
  where
    sideMap = M.fromList $ findSides <$> pairs (A.indices lsArray)
    lsArray = A.listArray (1, len) ls
    len = length ls

    findSides :: IP -> (IP, Side)
    findSides ip = (ip, side r $ points $ bimap (lsArray A.!) ip)
    
    populateHits :: Model -> Model
    populateHits m = Model r t (allHits m) ss ls
      where (Model r t _ ss ls) = m

    allHits :: Model -> [Hit]
    allHits m = L.sort $ hitsFromIps m $ pairs $ A.indices $ links m

    tieAll :: Model -> Model
    tieAll m = ties (innerIps m) m

    ties :: [IP] -> Model -> Model
    ties [] m = m
    ties (ip:ips) m = ties ips $ tie1 ip m

    tie1 :: IP -> Model -> Model
    tie1 ip m = replacePair m ip In $ buildLinks (points ls) (tie (chems ls))
      where
        ls = linksByI m ip

innerIps :: Model -> [IP]
innerIps m = innerIps' $ M.assocs $ sides m
  where
    innerIps' :: [(IP, Side)] -> [IP]
    innerIps' [] = []
    innerIps' ((ip, Out):ss) = innerIps' ss
    innerIps' ((ip, In):ss) = ip : innerIps' ss

linksByI :: Model -> IP -> Links
linksByI m = bimap (links m A.!)

sideByI :: Model -> IP -> Side
sideByI m = (sides m M.!)

replacePair :: Model -> IP -> Side -> Links -> Model
replacePair m ip s ls = Model r t (updateHits newM ip oldHs) newSS newLs
  where
    newM = Model r t oldHs (M.insert ip s oldSS) (oldLs A.// [(i1, l1), (i2, l2)])
    (Model _ _ _ newSS newLs) = newM
    (Model r t oldHs oldSS oldLs) = m
    (i1, i2) = ip
    (l1, l2) = ls

step :: Duration -> Model -> Model
step dt m = case nextHit m of
  Nothing -> moveModel dt m
  Just (Hit ht s ip) ->
    if endT < ht then moveModel dt m
    else step (endT - ht) $ bounceModel s ip $ moveModel (ht - startT) m
  where
    startT = time m
    endT = startT + dt

nextHit :: Model -> Maybe Hit
nextHit m = nextValidHit m $ hits m
  where
    nextValidHit :: Model -> [Hit] -> Maybe Hit
    nextValidHit _ [] = Nothing 
    nextValidHit m (h:hs) = if sideByI m ip == s then Just h else nextValidHit m hs
      where (Hit _ s ip) = h

updateHits :: Model -> IP -> [Hit] -> [Hit]
updateHits m ip hs = L.sort $ keep ++ newHits
  where
    keep = filter (uneffected ip) hs
    newHits = hitsFromIps m $ pairsOf (A.indices (links m)) ip

    uneffected :: IP -> Hit -> Bool
    uneffected ip h = not $(overlaps ip . ixPair) h

hitsFromIps :: Model -> [IP] -> [Hit]
hitsFromIps m = concatMap (toHits . times m)
  where
    times :: Model -> IP -> ([(Side, Duration)], IP)
    times m ip = (hitTimes (rad m) (time m) (points (linksByI m ip)), ip)
    toHits :: ([(Side, Duration)], IP) -> [Hit]
    toHits (hs, ip) = fmap (toHit ip) hs
    toHit :: IP -> (Side, Duration) -> Hit
    toHit ip (s, dt) = Hit dt s ip
  
moveModel :: Duration -> Model -> Model
moveModel dt (Model r t hs ss ls) = Model r (t+dt) hs ss ls

bounceModel :: Side -> IP -> Model -> Model
bounceModel s ip m = replacePair m ip newS $ buildLinks newPs newCs
  where
    ls = linksByI m ip
    ps = points ls
    (newS, newCs) = react (s, chems ls)
    newPs = if s == newS then bounceAt (time m) ps else ps

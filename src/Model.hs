module Model
( Model (Model)
, buildModel
, step
, nextHit
, moveModel
, hits
) where

import Data.Maybe (fromMaybe)
import qualified Data.Array as A
import qualified Data.Map as M
import Data.List (sort)
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
  , sides :: SideMap
  , links :: LinkArray
  }

-- Assumes all initial Chem's have: "has" == 0
buildModel :: Radius -> [Link] -> Model
buildModel r ls = tieAll $ Model r sideMap lsArray
  where
    sideMap = M.fromList $ findSides <$> pairs (A.indices lsArray)
    lsArray = A.listArray (1, len) ls
    len = length ls

    findSides :: IP -> (IP, Side)
    findSides ip = (ip, side r $ points $ bimap (lsArray A.!) ip)

    tieAll :: Model -> Model
    tieAll m = ties (inner (M.assocs (sides m))) m

    inner :: [(IP, Side)] -> [IP]
    inner [] = []
    inner ((ip, Out):ss) = inner ss
    inner ((ip, In):ss) = ip : inner ss

    ties :: [IP] -> Model -> Model
    ties [] m = m
    ties (ip:ips) m = ties ips $ tie1 ip m

    tie1 :: IP -> Model -> Model
    tie1 ip m = replacePair m In ip $ buildLinks (points ls) (tie (chems ls))
      where
        ls = linksByI m ip

linksByI :: Model -> IP -> Links
linksByI m = bimap (links m A.!)

sideByI :: Model -> IP -> Side
sideByI m = (sides m M.!)

replacePair :: Model -> Side -> IP -> Links -> Model
replacePair (Model r ss ls) s (i1, i2) (l1, l2) = Model r (M.insert (i1, i2) s ss) (ls A.// [(i1, l1), (i2, l2)])

step :: Duration -> Model -> Model
step dt m = case nextHit m of
  Nothing -> moveModel dt m
  Just (Hit ht s ip) ->
    if dt < ht then moveModel dt m
    else step (dt - ht) $ bounceModel s ip $ moveModel ht m

nextHit :: Model -> Maybe Hit
nextHit m = nextValidHit m $ hits m
  where
    nextValidHit :: Model -> [Hit] -> Maybe Hit
    nextValidHit _ [] = Nothing 
    nextValidHit m ((Hit dt s ip):hs) = if sideByI m ip == s then Just (Hit dt s ip) else nextValidHit m hs

hits :: Model -> [Hit]
hits m = sort $ concatMap (toHits . times m) ips
  where
    ips = pairs (A.indices (links m))
    times :: Model -> IP -> ([(Side, Duration)], IP)
    times m ip = (hitTimes (rad m) (points (linksByI m ip)), ip)
    toHits :: ([(Side, Duration)], IP) -> [Hit]
    toHits (hs, ip) = fmap (toHit ip) hs
    toHit :: IP -> (Side, Duration) -> Hit
    toHit ip (s, dt) = Hit dt s ip

moveModel :: Duration -> Model -> Model
moveModel dt (Model r ss ls) = Model r ss $ fmap (moveLink dt) ls

bounceModel :: Side -> IP -> Model -> Model
bounceModel s ip m = replacePair m newS ip $ buildLinks newPs newCs
  where
    ls = linksByI m ip
    ps = points ls
    (newS, newCs) = react (s, chems ls)
    newPs = if s == newS then bounce ps else ps

module Model
( Model (Model)
, buildModel
, step
, nextHit
, moveModel
, hits
) where

import Data.Maybe (fromMaybe)
import Data.Array
import Data.List (sort)
import Space
import Time
import Pair
import Points
import Chems
import Link
import Links

data Hit = Hit Duration Side IP deriving (Eq, Show)
instance Ord Hit where
  compare (Hit dt1 _ _) (Hit dt2 _ _) = compare dt1 dt2

dur :: Hit -> Duration
dur (Hit dt _ _) = dt

type LinkArray = Array Int Link
type IOArray = Array (Int, Int) Side
data Model = Model IOArray LinkArray

links :: Model -> LinkArray
links (Model _ ls) = ls

buildModel :: [Link] -> Model
buildModel ls = Model ioArray lsArray
  where
    ioArray = array ((1,1),(len,len)) $ initialSide <$> pairs (indices lsArray)
    initialSide ip = (ip, Out) -- Hardcoding all of them to Out, for now
    lsArray = listArray (1, len) ls
    len = length ls

moveModel :: Duration -> Model -> Model
moveModel dt (Model ss ls) = Model ss $ fmap (moveLink dt) ls

linksByI :: Model -> IP -> Links
linksByI (Model _ ls) = bimap (ls !)

sideByI :: Model -> IP -> Side
sideByI (Model ss _) = (ss !)

replaceLinks :: Model -> Side -> IP -> Links -> Model
replaceLinks (Model ss ls) s (i1, i2) (l1, l2) = Model (ss // [((i1, i2), s)]) (ls // [(i1, l1), (i2, l2)])

step :: Duration -> Radius -> Model -> Model
step dt rad m = case nextHit rad m of
  Nothing -> moveModel dt m
  Just (Hit ht s ip) ->
    if dt < ht then moveModel dt m
    else step (dt - ht) rad $ bounceModel s ip $ moveModel ht m

nextHit :: Radius -> Model -> Maybe Hit
nextHit rad m = nextValidHit m $ hits rad m
  where
    nextValidHit :: Model -> [Hit] -> Maybe Hit
    nextValidHit _ [] = Nothing 
    nextValidHit m ((Hit dt s ip):hs) = if sideByI m ip == s then Just (Hit dt s ip) else nextValidHit m hs

hits :: Radius -> Model -> [Hit]
hits rad m = sort $ concatMap (toHits . times m) ips
  where
    ips = pairs (indices (links m))
    times :: Model -> IP -> ([(Side, Duration)], IP)
    times m ip = (hitTimes rad (points (linksByI m ip)), ip)
    toHits :: ([(Side, Duration)], IP) -> [Hit]
    toHits (hs, ip) = fmap (toHit ip) hs
    toHit :: IP -> (Side, Duration) -> Hit
    toHit ip (s, dt) = Hit dt s ip

bounceModel :: Side -> IP -> Model -> Model
bounceModel s ip m = replaceLinks m newS ip $ buildLinks newPs newCs
  where
    ls = linksByI m ip
    ps = points ls
    (newS, newCs) = react (s, chems ls)
    newPs = if s == newS then bounce ps else ps

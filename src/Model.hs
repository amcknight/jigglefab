module Model
( Model
, step
, nextHit
, moveModel
) where

import Data.Maybe (fromMaybe)
import Points
import Chems
import Link
import Links
import Space
import Pair

type Model = [Link]

step :: Duration -> Radius -> Model -> Model
step dt rad m = case nextHit rad m of
  Nothing -> moveModel dt m
  Just (i, ht) ->
    if dt < ht then moveModel dt m
    else stepWithContact dt ht i rad m

stepWithContact :: Duration -> Duration -> IP -> Radius -> Model -> Model
stepWithContact dt ht is rad m = step (dt - sht) rad newM
  where
    ls = bimap (m !!) is
    ps = points ls
    s = side rad ps
    (Products hit newCs) = react $ Reactants s $ chems ls
    newSide = updateSide s hit
    sht = sidedHitTime ht newSide rad ps
    
    m2 = moveModel sht m
    (left, ml1, middle, ml2, right) = trisectAt is m2
    mps = points (ml1, ml2)
    newPs = case hit of 
      Pass -> mps
      Bounce -> bounce mps
    (newL1, newL2) = buildLinks newPs newCs
    newM = left ++ (newL1:middle ++ (newL2:right))

trisectAt :: IP -> [Link] -> ([Link], Link, [Link], Link, [Link])
trisectAt (i1, i2) links = (left, l1, middle, l2, right)
  where
    (left, l1:midRight) = splitAt i1 links
    (middle, l2:right) = splitAt (i2-i1-1) midRight

moveModel :: Duration -> Model -> Model
moveModel dt = fmap (moveLink dt)

nextHit :: Radius -> Model -> Maybe (IP, Duration)
nextHit rad m = least $ hitTimes rad m
  where
    least :: [(IP, Duration)] -> Maybe (IP, Duration)
    least [] = Nothing
    least [id] = Just id
    least (id:ids) = Just $ foldr leastOf id ids
    leastOf :: (IP, Duration) -> (IP, Duration) -> (IP, Duration)
    leastOf (i1, d1) (i2, d2) = if d1 < d2 then (i1, d1) else (i2, d2)

hitTimes :: Radius -> Model -> [(IP, Duration)]
hitTimes rad = fmap removeJust . filter existing . allHitTimes rad
  where
    existing :: (IP, Maybe Duration) -> Bool
    existing (i, Nothing) = False
    existing _ = True
    removeJust :: (IP, Maybe Duration) -> (IP, Duration)
    removeJust (i, Nothing) = undefined -- Should never happen because of "existing"
    removeJust (i, Just dt) = (i, dt)

allHitTimes :: Radius -> Model -> [(IP, Maybe Duration)]
allHitTimes rad m = fmap time (indexedPairs m)
  where
    time :: (IP, Links) -> (IP, Maybe Duration)
    time (i, ls) = (i, hitTime rad (points ls))

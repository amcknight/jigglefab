module Model
( Model (Model)
, step
) where

import Data.Maybe (fromMaybe)
import Points
import Chems
import Link
import Links
import Space

data Model = Model Radius [Link] deriving Show
type IP = (Int, Int) -- Index Pair

step :: Duration -> Model -> Model
step dt m = case nextHit m of
  Nothing -> move dt m
  Just (i, ht) ->
    if dt < ht then move dt m
    else stepWithContact dt ht i m

stepWithContact :: Duration -> Duration -> IP -> Model -> Model
stepWithContact dt ht (i1, i2) (Model rad links) = step (dt - sht) (Model rad2 newLinks)
  where
    ls = (links !! i1, links !! i2)
    ps = points ls
    s = side rad ps
    (Products hit newCs) = react $ Reactants s $ chems ls
    newSide = updateSide s hit
    sht = sidedHitTime ht newSide rad ps
    
    (Model rad2 movedLinks) = move sht (Model rad links)
    (left, ml1:midRight) = splitAt i1 movedLinks
    (middle, ml2:right) = splitAt (i2-i1-1) midRight
    mps = points (ml1, ml2)
    newPs = case hit of 
      Pass -> mps
      Bounce -> bounce mps
    (newL1, newL2) = buildLinks newPs newCs
    newLinks = left ++ (newL1:middle ++ (newL2:right))

move :: Duration -> Model -> Model
move dt (Model rad links) = Model rad $ fmap (moveLink dt) links

nextHit :: Model -> Maybe (IP, Duration)
nextHit m = least $ hitTimes m
  where
    least :: [(IP, Duration)] -> Maybe (IP, Duration)
    least [] = Nothing
    least [id] = Just id
    least (id:ids) = Just $ foldr leastOf id ids
    leastOf :: (IP, Duration) -> (IP, Duration) -> (IP, Duration)
    leastOf (i1, d1) (i2, d2) = if d1 < d2 then (i1, d1) else (i2, d2)

hitTimes :: Model -> [(IP, Duration)]
hitTimes = fmap removeJust . filter existing . allHitTimes
  where
    existing :: (IP, Maybe Duration) -> Bool
    existing (i, Nothing) = False
    existing _ = True
    removeJust :: (IP, Maybe Duration) -> (IP, Duration)
    removeJust (i, Nothing) = undefined -- Should never happen because of "existing"
    removeJust (i, Just dt) = (i, dt)

allHitTimes :: Model -> [(IP, Maybe Duration)]
allHitTimes (Model rad links) = fmap time (indexedLinkPairs links)
  where
    time :: (IP, Links) -> (IP, Maybe Duration)
    time (i, ls) = (i, hitTime rad (points ls))

indexedLinkPairs :: [Link] -> [(IP, Links)]
indexedLinkPairs links = filter proper allPairs
  where
    proper :: (IP, Links) -> Bool
    proper ((i1, i2), ls) = i1 < i2
    allPairs = do
      let iLinks = zip [0..] links
      (i1, l1) <- iLinks
      (i2, l2) <- iLinks
      return ((i1, i2), (l1, l2))

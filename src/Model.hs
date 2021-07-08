module Model
( Link
, Model (Model)
, step
) where

import Data.Maybe (fromMaybe)
import Link 
import Vector
import Point
import Space

data Model = Model Radius [Link] deriving Show
type IP = (Int, Int) -- Index Pair

step :: Duration -> Model -> Model
step dt m = case nextHit m of
  Nothing -> move dt m
  Just (i, ht) ->
    if dt < ht then move dt m
    else step (dt - ht) (bounceModel i (move ht m))

move :: Duration -> Model -> Model
move dt (Model rad links) = Model rad $ fmap (moveLink dt) links

bounceModel :: IP -> Model -> Model
bounceModel (i1, i2) (Model rad links) = Model rad newLinks
  where
    newLinks = left ++ (newL1:middle ++ (newL2:right))
    (left, l1:midRight) = splitAt i1 links
    (middle, l2:right) = splitAt (i2-i1-1) midRight
    (newL1, newL2) = bounce l1 l2

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
hitTimes = filter coolDown . fmap removeJust . filter existing . allHitTimes
  where
    existing :: (IP, Maybe Duration) -> Bool
    existing (i, Nothing) = False
    existing (i, Just d) = True
    removeJust :: (IP, Maybe Duration) -> (IP, Duration)
    removeJust (i, Nothing) = undefined 
    removeJust (i, Just d) = (i, d)
    coolDown :: (IP, Duration) -> Bool
    coolDown (i, dt) = dt > 0.00001

allHitTimes :: Model -> [(IP, Maybe Duration)]
allHitTimes (Model rad links) = fmap time (indexedLinkPairs links)
  where
    time :: (IP, (Link, Link)) -> (IP, Maybe Duration)
    time (i, (l1, l2)) = (i, hitTime rad l1 l2)

indexedLinkPairs :: [Link] -> [(IP, (Link, Link))]
indexedLinkPairs links = filter proper allPairs
  where
    proper :: (IP, (Link, Link)) -> Bool
    proper ((i1, i2), ls) = i1 < i2
    allPairs = do
      let iLinks = zip [0..] links
      (i1, l1) <- iLinks
      (i2, l2) <- iLinks
      return ((i1, i2), (l1, l2))

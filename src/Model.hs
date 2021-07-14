module Model
( Model (Model)
, step
) where

import Data.Maybe (fromMaybe)
import Points
import Link
import Links
import Space

data Model = Model Radius [Link] deriving Show
type IP = (Int, Int) -- Index Pair

step :: Duration -> Model -> Model
step dt m = case nextHit m of
  Nothing -> move dt m
  Just (i, w, ht) ->
    if dt < ht then move dt m
    else step (dt - ht) (bounceModel i w (move ht m))

move :: Duration -> Model -> Model
move dt (Model rad links) = Model rad $ fmap (moveLink dt) links

bounceModel :: IP -> Side -> Model -> Model
bounceModel (i1, i2) s (Model rad links) = Model rad newLinks
  where
    newLinks = left ++ (newL1:middle ++ (newL2:right))
    (left, l1:midRight) = splitAt i1 links
    (middle, l2:right) = splitAt (i2-i1-1) midRight
    (newL1, newL2) = contact s (l1, l2)

nextHit :: Model -> Maybe (IP, Side, Duration)
nextHit m = least $ hitTimes m
  where
    least :: [(IP, Side, Duration)] -> Maybe (IP, Side, Duration)
    least [] = Nothing
    least [id] = Just id
    least (id:ids) = Just $ foldr leastOf id ids
    leastOf :: (IP, Side, Duration) -> (IP, Side, Duration) -> (IP, Side, Duration)
    leastOf (i1, s1, d1) (i2, s2, d2) = if d1 < d2 then (i1, s1, d1) else (i2, s2, d2)

hitTimes :: Model -> [(IP, Side, Duration)]
hitTimes = filter coolDown . fmap removeJust . filter existing . allHitTimes
  where
    existing :: (IP, Maybe (Side, Duration)) -> Bool
    existing (i, Nothing) = False
    existing _ = True
    removeJust :: (IP, Maybe (Side, Duration)) -> (IP, Side, Duration)
    removeJust (i, Nothing) = undefined -- Should never happen because of "existing"
    removeJust (i, Just (w, d)) = (i, w, d)
    coolDown :: (IP, Side, Duration) -> Bool
    coolDown (i, w, dt) = dt > 0.00001 -- TODO: This is a hack

allHitTimes :: Model -> [(IP, Maybe (Side, Duration))]
allHitTimes (Model rad links) = fmap time (indexedLinkPairs links)
  where
    time :: (IP, Links) -> (IP, Maybe (Side, Duration))
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

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
  Just (Hit ht ls subM) ->
    if dt < ht then moveModel dt m
    else step (dt - ht) rad $ stepWithContact ht rad ls subM

stepWithContact :: Duration -> Radius -> Links -> Model -> Model
stepWithContact ht rad ls subM = newM
  where
    (Products cont newCs, sht) = prereact rad ls ht
    newSubM = moveModel sht subM
    movedPs = movePoints sht $ points ls
    newPs = case cont of 
      Pass -> movedPs
      Bounce -> bounce movedPs
    pairM = asList $ buildLinks newPs newCs
    newM = pairM ++ newSubM

prereact :: Radius -> Links -> Duration -> (Products, Duration)
prereact rad ls ht = (prods, sht)
  where
    ps = points ls
    s = side rad ps
    prods = react $ Reactants s $ chems ls
    newSide = updateSide s $ contact prods
    sht = sidedHitTime ht newSide rad ps

moveModel :: Duration -> Model -> Model
moveModel dt = fmap (moveLink dt)

type Hit = (Duration, Links, Model)

nextHit :: Radius -> Model -> Maybe Hit
nextHit rad m = least $ hits rad m
  where
    least :: [Hit] -> Maybe Hit
    least [] = Nothing
    least (id:ids) = Just $ foldr leastOf id ids
    leastOf :: Hit -> Hit -> Hit
    leastOf (d1, ls1, m1) (d2, ls2, m2) = if d1 < d2 then (d1, ls1, m1) else (d2, ls2, m2)

hits :: Radius -> Model -> [Hit]
hits rad m = exists $ time <$> pairsUp m
  where
    time :: (Links, Model) -> (Maybe Duration, Links, Model)
    time (ls, m) = ((hitTime rad . points) ls, ls, m)
    exists :: [(Maybe Duration, Links, Model)] -> [Hit]
    exists [] = []
    exists ((Nothing, _, _):xs) = exists xs
    exists ((Just dt, ls, m):xs) = (dt, ls, m) : exists xs

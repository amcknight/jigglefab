module Model
( Model (Model, rad, walls, wSides, hSides, balls)
, buildModel
, wSideByI, hSideByI, ballsByI
, step
, moveModel
, hits
, innerIps
, updateHits
) where

import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.List as L
import Space
import Time
import Pair
import Point
import Points
import Chems
import Ball
import Balls
import Hit
import Wall
import Vector

type BallVector = V.Vector Ball
type WallVector = V.Vector Wall
type SideMap = M.Map (Int, Int) Side
data Model = Model
  { rad :: Radius
  , walls :: WallVector
  , balls :: BallVector
  , wSides :: SideMap
  , hSides :: SideMap
  , hits :: [Hit]
  } deriving Show

buildModel :: Radius -> [Wall] -> [Ball] -> Model
buildModel r ws ls = tieAll $ populateHits $ Model r wsVector lsVector wss hss []
  where
    wsVector = V.fromList ws
    lsVector = V.fromList ls
    wLen = length ws
    hLen = length ls
    wss = M.fromList $ findWSides <$> prodTo wLen hLen 
    hss = M.fromList $ findHSides <$> pairsTo hLen

    findWSides :: IP -> (IP, Side)
    findWSides (wi, li) = case ortho w of
      Vertical -> case compare (place w) x of
        LT -> ((wi, li), Out)
        _ -> ((wi, li), In)
      Horizontal -> case compare (place w) y of
        LT -> ((wi, li), Out)
        _ -> ((wi, li), In)
      where
        (x, y) = coords (pos (point l))
        w = wsVector V.! wi
        l = lsVector V.! li

    findHSides :: IP -> (IP, Side)
    findHSides ip = (ip, side r $ points $ bimap (lsVector V.!) ip)
    
    populateHits :: Model -> Model
    populateHits m = Model r wsVector lsVector wss hss (allHits m)
      where (Model r _ _ wss hss ls) = m

    allHits :: Model -> [Hit]
    allHits m = L.sort $ hitsFromIps m $ pairsTo hLen

    tieAll :: Model -> Model
    tieAll m = ties (innerIps m) m

    ties :: [IP] -> Model -> Model
    ties [] m = m
    ties (ip:ips) m = ties ips $ tie1 ip m

    tie1 :: IP -> Model -> Model
    tie1 ip m = replacePair m ip In $ buildBalls (points ls) (tie (chems ls))
      where
        ls = ballsByI m ip

innerIps :: Model -> [IP]
innerIps m = innerIps' $ M.assocs $ hSides m
  where
    innerIps' :: [(IP, Side)] -> [IP]
    innerIps' [] = []
    innerIps' ((ip, Out):ss) = innerIps' ss
    innerIps' ((ip, In):ss) = ip : innerIps' ss

ballByI :: Model -> Int -> Ball
ballByI m = (balls m V.!)

wallByI :: Model -> Int -> Wall
wallByI m = (walls m V.!)

ballsByI :: Model -> IP -> Balls
ballsByI = bimap . ballByI

hSideByI :: Model -> IP -> Side
hSideByI m = (hSides m M.!)

wSideByI :: Model -> IP -> Side
wSideByI m = (wSides m M.!)

replace :: Model -> Int -> Ball -> Model
replace m i l = Model r ws newLs oldWss oldHss $ updateHits1 newM i oldHs
  where
    newM = Model r ws (oldLs V.// [(i, l)]) oldWss oldHss oldHs
    (Model _ _ newLs _ _ _) = newM
    (Model r ws oldLs oldWss oldHss oldHs) = m

replacePair :: Model -> IP -> Side -> Balls -> Model
replacePair m ip s ls = Model r ws newLs wss newHss $ updateHits newM ip oldHs
  where
    newM = Model r ws (oldLs V.// [(i1, l1), (i2, l2)]) wss (M.insert ip s oldHss) oldHs
    (Model _ _ newLs _ newHss _) = newM
    (Model r ws oldLs wss oldHss oldHs) = m
    (i1, i2) = ip
    (l1, l2) = ls

step :: Duration -> Model -> Model
step dt m = case (nextBonk m, nextBounce m) of
  (Nothing, Nothing) -> moveModel dt m
  (Nothing, Just (Hit bt s ip)) ->
    if dt < bt then moveModel dt m
    else step (dt - bt) $ bounceModel s ip $ moveModel bt m
  (Just (Hit bt s wlip), Nothing) ->
    if dt < bt then moveModel dt m
    else step (dt - bt) $ bonkModel s wlip $ moveModel bt m
  (Just (Hit bnkTime bs wlip), Just (Hit bncTime hs ip)) -> case compare bnkTime bncTime of
    LT ->
      if dt < bnkTime then moveModel dt m
      else step (dt - bnkTime) $ bonkModel bs wlip $ moveModel bnkTime m
    GT ->
      if dt < bncTime then moveModel dt m
      else step (dt - bncTime) $ bounceModel hs ip $ moveModel bncTime m
    EQ ->
      if dt < bncTime then moveModel dt m
      else step (dt - bncTime) $ bounceModel hs ip $ bonkModel bs wlip $ moveModel bncTime m

nextBonk :: Model -> Maybe Hit
nextBonk m = nextValidBonk m bonks
    where
      bonks = L.sort (mapMaybe (toBonk m) (prodTo (length ws) (length ls)))
      Model rad ws ls _ _ _ = m
      nextValidBonk :: Model -> [Hit] -> Maybe Hit
      nextValidBonk _ [] = Nothing 
      nextValidBonk m (b:bs) = if wSideByI m ip == s then Just b else nextValidBonk m bs
        where Hit _ s ip = b
      
nextBounce :: Model -> Maybe Hit
nextBounce m = nextValidBounce m $ hits m
  where
    nextValidBounce :: Model -> [Hit] -> Maybe Hit
    nextValidBounce _ [] = Nothing 
    nextValidBounce m (h:hs) = if hSideByI m ip == s then Just h else nextValidBounce m hs
      where (Hit _ s ip) = h

updateHits1 :: Model -> Int -> [Hit] -> [Hit]
updateHits1 m i hs = L.sort $ keep ++ newHits
  where
    keep = filter (uneffected i) hs
    newHits = hitsFromIps m $ pairsOfTo1 (length (balls m)) i

    uneffected :: Int -> Hit -> Bool
    uneffected i h = not $ (overlaps1 i . Hit.ixPair) h

updateHits :: Model -> IP -> [Hit] -> [Hit]
updateHits m ip hs = L.sort $ keep ++ newHits
  where
    keep = filter (uneffected ip) hs
    newHits = hitsFromIps m $ pairsOfTo (length (balls m)) ip

    uneffected :: IP -> Hit -> Bool
    uneffected ip h = not $ (overlaps ip . Hit.ixPair) h

hitsFromIps :: Model -> [IP] -> [Hit]
hitsFromIps m = concatMap (toHits . times m)
  where
    times :: Model -> IP -> ([(Side, Duration)], IP)
    times m ip = (hitTimes (rad m) (points (ballsByI m ip)), ip)
    toHits :: ([(Side, Duration)], IP) -> [Hit]
    toHits (hs, ip) = fmap (toHit ip) hs
    toHit :: IP -> (Side, Duration) -> Hit
    toHit ip (s, dt) = Hit dt s ip
  
moveModel :: Duration -> Model -> Model
moveModel dt (Model r ws ls wss hss hs) = Model r ws (fmap (moveBall dt) ls) wss hss (mapMaybe (moveHit dt) hs)

bounceModel :: Side -> IP -> Model -> Model
bounceModel s ip m = replacePair m ip newS $ buildBalls newPs newCs
  where
    ls = ballsByI m ip
    ps = points ls
    cs = chems ls
    (newS, newCs) = react (s, cs)
    newPs = if s == newS then bounce ps else ps

bonkModel :: Side -> IP -> Model -> Model
bonkModel s (wi, li) m = replace m li newBall
  where
    Ball p c = ballByI m li
    Wall o _ = wallByI m wi
    newBall = Ball (bonk o p) c

toBonk :: Model -> IP -> Maybe Hit
toBonk m wlip = case compare t 0 of
  GT -> Just $ Hit t ws wlip
  _ -> Nothing
  where
    (wi, li) = wlip
    w = wallByI m wi
    ws = wSideByI m wlip
    Ball p _ = ballByI m li
    t = intersectTime w p
  
intersectTime :: Wall -> Point -> Time
intersectTime (Wall o p) (Point (V x y) (V xv yv)) = case o of
  Vertical -> -(x-p)/xv
  Horizontal -> -(y-p)/yv

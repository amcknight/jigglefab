module Model
( Model (Model, rad, form, wSides, hSides)
, buildModel
, wSideByI, hSideByI, ballsByI
, step
, moveModel
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
import Points
import Chems
import Ball
import Balls
import Hit
import Wall
import Vector
import Form

type SideMap = M.Map (Int, Int) Side
data Model = Model
  { rad :: Radius
  , form :: Form
  , wSides :: SideMap
  , hSides :: SideMap
  , hits :: [Hit]
  } deriving Show

buildModel :: Radius -> Form -> Model
buildModel rad f = tieAll $ populateHits $ Model rad f wss hss []
  where
    wss = M.fromList $ findWSides f <$> bonkIndices f
    hss = M.fromList $ findHSides rad f <$> bounceIndices f

    findWSides :: Form -> IP -> (IP, Side)
    findWSides f (wi, bi) = case o of
      Vertical -> case compare pl x of
        LT -> ((wi, bi), Out)
        _ -> ((wi, bi), In)
      Horizontal -> case compare pl y of
        LT -> ((wi, bi), Out)
        _ -> ((wi, bi), In)
      where
        (x, y) = coords (pos p)
        Wall o pl = wallByI f wi
        Ball p _ = ballByI f bi

    findHSides :: Radius -> Form -> IP -> (IP, Side)
    findHSides rad f ip = (ip, hSide)
      where hSide = side rad $ points $ bimap (ballByI f) ip
    
    populateHits :: Model -> Model
    populateHits (Model r f wss hss _) = Model r f wss hss $ L.sort $ hitsFromIps r f $ bounceIndices f

    tieAll :: Model -> Model
    tieAll m = ties (innerIps m) m

    ties :: [IP] -> Model -> Model
    ties [] m = m
    ties (ip:ips) m = ties ips $ tie1 ip m

    tie1 :: IP -> Model -> Model
    tie1 ip m = replacePair m ip In $ buildBalls (points bs) (tie (chems bs))
      where
        bs = ballsByI (form m) ip

innerIps :: Model -> [IP]
innerIps m = innerIps' $ M.assocs $ hSides m
  where
    innerIps' :: [(IP, Side)] -> [IP]
    innerIps' [] = []
    innerIps' ((ip, Out):ss) = innerIps' ss
    innerIps' ((ip, In):ss) = ip : innerIps' ss

hSideByI :: Model -> IP -> Side
hSideByI m = (hSides m M.!)

wSideByI :: Model -> IP -> Side
wSideByI m = (wSides m M.!)

replace :: Model -> Int -> Ball -> Model
replace (Model r oldF wss hss oldHs) i b = Model r newF wss hss $ updateHits1 r newF i oldHs
  where newF = replaceBall i b oldF

replacePair :: Model -> IP -> Side -> Balls -> Model
replacePair (Model r oldF wss hss oldHs) ip s bs = Model r newF wss hss $ updateHits2 r newF ip oldHs
  where newF = replaceBalls ip bs oldF

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
    bonks = L.sort (mapMaybe (toModelBonk m) (bonkIndices f))
    Model rad f _ _ _ = m
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

updateHits1 :: Radius -> Form -> Int -> [Hit] -> [Hit]
updateHits1 r f i hs = L.sort $ keep ++ newHits
  where
    keep = filter (uneffected i) hs
    newHits = hitsFromIps r f $ pairsOfTo1 (length (balls f)) i

    uneffected :: Int -> Hit -> Bool
    uneffected i h = not $ (overlaps1 i . ixPair) h

updateHits2 :: Radius -> Form -> IP -> [Hit] -> [Hit]
updateHits2 r f ip hs = L.sort $ keep ++ newHits
  where
    keep = filter (uneffected ip) hs
    newHits = hitsFromIps r f $ pairsOfTo2 (length (balls f)) ip

    uneffected :: IP -> Hit -> Bool
    uneffected ip h = not $ (overlaps2 ip . ixPair) h

hitsFromIps :: Radius -> Form -> [IP] -> [Hit]
hitsFromIps r f = concatMap (toHits . times r f)
  where
    times :: Radius -> Form -> IP -> ([(Side, Duration)], IP)
    times r f ip = (hitTimes r (points (ballsByI f ip)), ip)
    toHits :: ([(Side, Duration)], IP) -> [Hit]
    toHits (hs, ip) = fmap (toHit ip) hs
    toHit :: IP -> (Side, Duration) -> Hit
    toHit ip (s, dt) = Hit dt s ip
  
moveModel :: Duration -> Model -> Model
moveModel dt (Model r f wss hss hs) = Model r (moveForm dt f) wss hss (mapMaybe (moveHit dt) hs)

bounceModel :: Side -> IP -> Model -> Model
bounceModel s ip m = replacePair m ip newS $ buildBalls newPs newCs
  where
    bs = ballsByI (form m) ip
    ps = points bs
    cs = chems bs
    (newS, newCs) = react (s, cs)
    newPs = if s == newS then bounce ps else ps

bonkModel :: Side -> IP -> Model -> Model
bonkModel s (wi, li) m = replace m li newBall
  where
    f = form m
    Wall o _ = wallByI f wi
    Ball p c = ballByI f li
    newBall = Ball (bonk o p) c

toModelBonk :: Model -> IP -> Maybe Hit
toModelBonk m ip = toBonk (form m) (wSideByI m ip) ip

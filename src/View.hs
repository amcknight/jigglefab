module View
  ( View(..)
  , panHop, zoomHop
  , togglePlay
  , setOverlayOn
  , click, rightClick
  )
where

import Model
import Geometry.Vector
import Struct
import Geometry.Space
import Form
import System.Random
import Time
import Control.Monad.State
import Chem
import Debug.Trace
import Overlay
import DataType
import Pair
import Orb

data View c = View
  { structOrModel :: Either (Struct c) (Model c)
  , seed :: StdGen
  , overlay :: Overlay
  , tip :: Token
  , center :: Position
  , zoom :: Double
  }

instance HasPos (View c) where
  pos = center

panHop :: Geometry.Vector.Vector -> View c -> View c
panHop dirV view = view {center = center view |+ (hop |* dirV)}
  where hop = 150

zoomHop :: Side -> View c -> View c
zoomHop s view = case s of
  Out -> view {zoom = zoom view * zhop}
  In -> view {zoom = zoom view * (1/zhop)}
  where zhop = 1.25

togglePlay :: Chem c => Speed -> View c -> View c
togglePlay sp view = case structOrModel view of
  Left s ->  view {structOrModel = Right (evalState (buildModel sp s) (seed view))}
  Right m -> view {structOrModel = Left (extractStruct (form m))}

setOverlayOn :: Position -> View c -> View c
setOverlayOn p view = view {overlay = Overlay p []}

click :: Position -> Con -> View c -> View c
click mpos c v = case overlay v of
  NoOverlay -> case structOrModel v of
    Left st -> case getCon c $ tip v of
      Nothing -> error $ "Invalid TIP: " ++ show (tip v)
      Just con -> v -- v {structOrModel = Left $ addOrb (Orb mpos con) st} TODO: This will only work if using Metachem instead of chem everywhere
    Right mo -> v
  Overlay _ tk -> if isLeafAt c tk
    then v {overlay = NoOverlay, tip = tk}
    else v {overlay = NoOverlay}

rightClick :: Position -> View c -> View c
rightClick mpos v = case structOrModel v of
  Left _ -> setOverlayOn mpos v
  Right _ -> v {overlay = NoOverlay}

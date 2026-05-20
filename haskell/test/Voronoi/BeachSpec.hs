module Voronoi.BeachSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck
import Geometry.Vector
import Voronoi.Beach
import Voronoi.Event
import Voronoi.Edge
import Geometry.Angle
import Util.Utils
import Geometry.Circle
import Data.Maybe (isJust)

instance Arbitrary Bouy where
  arbitrary :: Gen Bouy
  arbitrary = Bouy <$> arbitrary <*> arbitrary

spec :: Spec
spec = do
  describe "Rays" $ do
    prop "New Rays emit from center" $
      \b1 b2 b3 ->
        let mc = circleFrom3 (pos b1) (pos b2) (pos b3)
            Just (Circle p _) = mc
            rs = newRays p b1 b2 b3
        in isJust mc ==> length rs == 3 && and (fmap (\r -> pos r == p) rs)
    prop "Away Rays are perpendicular to line between generating points" $
      \a p q -> not (colinear 5 [a,p,q]) ==> 
        let Just (Circle o _) = circleFrom3 a p q
            Just baseDir = direction $ p |- q
            rayDir = awayRay o a p q
        in near 5 (simple (rayDir+0.25)) baseDir || near 5 (simple (rayDir-0.25)) baseDir

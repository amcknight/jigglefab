module Voronoi.BeachSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck
import Geometry.Vector
import Voronoi.Beach
import Voronoi.Event
import Voronoi.Edge
import Geometry.Angle

instance Arbitrary Bouy where
  arbitrary = Bouy <$> arbitrary <*> arbitrary

spec :: Spec
spec = do
  describe "Rays" $ do
    prop "New Rays emit from center" $
      \p b1 b2 b3 ->
        let rs = newRays p b1 b2 b3
        in length rs == 3 && and (fmap (\r -> pos r == p) rs)
    prop "Away Rays are perpendicular to line between generating points" $
      \o a p q -> (o /= a && o /= p && o /= q) ==> separation (awayRay o a p q) (direction (p |- q)) `shouldBe` Orthogonal

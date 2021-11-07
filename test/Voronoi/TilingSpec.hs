module Voronoi.TilingSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Test.QuickCheck
import Geometry.Vector
import Geometry.CrossPoint
import Voronoi.Tiling
import Data.Maybe (isNothing)

spec :: Spec
spec = do
  describe "Tiling Tris" $ do
    prop "Edge that would intersect circle but is not overlapping creates no Tri" $
      \p gap ->
        let [p1, p2, c1, c2] = fromBy p gap 4
            o = mid p2 c1 |+ rotate (p2 |- c1) 0.25 -- Arbitrary non-colinear point
        in  (magnitudeSq gap > 0) ==> buildTri p1 p2 (TwoCross c1 c2) o `shouldSatisfy` isNothing

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
      \p gap o ->
        let [p1, p2, c1, c2] = fromBy p gap 4
        in  (magnitudeSq gap > 0 && not (colinear o p1 p2)) ==> buildTri p1 p2 (TwoCross c1 c2) o `shouldSatisfy` isNothing

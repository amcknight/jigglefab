module Voronoi.FortuneSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.List (sort)
import Voronoi.Fortune
import Geometry.Angle
import Test.QuickCheck
import Geometry.Vector
import Voronoi.Edge
import Geometry.Line
import Debug.Trace
import Utils

spec :: Spec
spec = do
  describe "Voronoi" $ do
    prop "Single Point created empty Voronoi" $
      \p -> voronoi [p] `shouldSatisfy` null
    prop "Pair of points creates one edge" $
      \p q -> (p /= q) ==> length (voronoi [p,q]) `shouldBe` 1
    prop "Two edges from 3 colinear points" $
      \pos scale turn ->
        let gap = scale |* unitV turn
            ps = fmap ((pos |+) . (|* gap)) [0, 1, 2]
        in (scale /= 0.0) ==> length (voronoi ps) `shouldBe` 2
    prop "Rotated points voronoi is same as rotated voronoi points" $
      \c ps t ->
        let edgePoints = concatMap (\(Edge (Seg p1 p2) _) -> [p1, p2])
            vres = sort $ edgePoints (voronoi (fmap (rotateAround c t) ps))
            rves = sort $ fmap (rotateAround c t) (edgePoints (voronoi ps))
        in not (anyEq ps) ==> all (uncurry (near 5)) $ zip vres rves

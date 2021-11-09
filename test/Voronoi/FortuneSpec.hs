module Voronoi.FortuneSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Data.List (sort, permutations)
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
    prop "Voronoi of Points should be same as reversed points" $
      \ps -> not (anyEq ps) ==> and $ zipWith (near 5) (edgePoints (voronoi ps)) (edgePoints (voronoi (reverse ps)))

edgePoints :: [Edge] -> [Position]
edgePoints = sort . concatMap (\(Edge (Seg p1 p2) _) -> [p1, p2])

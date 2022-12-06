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
import Util.Utils
import Data.Maybe (fromMaybe)

spec :: Spec
spec = do
  describe "Voronoi" $ do
    prop "Single Point created empty Voronoi" $
      \p -> voronoi [p] `shouldSatisfy` null
    prop "Pair of points creates one edge" $
      \p q -> p /= q ==> length (voronoi [p,q]) `shouldBe` 1
    prop "Two edges from 3 colinear points" $
      \pos scale turn ->
        let gap = scale |* unitV turn
            ps = fmap ((pos |+) . (|* gap)) [0, 1, 2]
        in scale /= 0.0 ==> length (voronoi ps) `shouldBe` 2
    prop "Voronoi of Points should be same as reversed points" $
      \ps -> not (anyEq ps) ==> and $ zipWith (near 5) (edgePoints (voronoi ps)) (edgePoints (voronoi (reverse ps)))
    prop "Jittered Horizontal line creates an edge between each vertex" $
      \x1 x2 n js ->
        let jScale = 0.01*abs(x1 - x2)/fromIntegral n
            scaledJs = fmap (\j -> jScale |* fromMaybe zeroV (unit j)) js
            ps = zipWith (|+) (fromTo (x1,0) (x2,0) n) (take n (cycle scaledJs))
        in x1 /= x2 && n > 0 && not (null js) ==> length (voronoi ps) `shouldBe` (n-1)
    prop "Points on a Circle" $
      \c r ts ->
        let sts = fmap simple ts
            ps = fmap (\t -> c |+ (r |* unitV t)) sts
            outsidePs = filter (\p -> magnitudeSq (p |- c) >= r*0.999) (edgePoints (voronoi ps))
        in r > 0 && length ts > 1 && not (anyEq sts) ==> length outsidePs `shouldBe` length ts
    prop "Shifted middle two should create trident or 3 lines" $
      \gap shift ->
        let ps = [(0*gap,0), (1*gap,shift), (2*gap,shift), (3*gap,0)]
            eps = edgePoints (voronoi ps)
        in length eps == 4 || length eps == 6
edgePoints :: [Edge] -> [Position]
edgePoints = sort . concatMap (\(Edge (Seg p1 p2) _) -> [p1, p2])

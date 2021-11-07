module Geometry.VectorSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Geometry.Vector
import SpecUtils
import Utils

spec :: Spec
spec = do
  describe "Colinear Vectors" $ do
    prop "Midpoint is colinear with two given vectors" $
      \p q -> colinear p q $ mid p q
    prop "Lines have colinear vectors" $
      \p gap ->
        let [p1,p2,p3] = fromBy p gap 3
        in (magnitudeSq gap > 0) ==> colinear p1 p2 p3
  describe "Sums" $ do
    prop "Sum of two vectors is sum of the midpoint twice" $
      \p q -> p |+ q `shouldBe` 2 |* mid p q
    prop "Symmetric sum" $
      sym2 (|+)
  describe "Unit Vectors" $
    prop "Scaled unit vector is same as the original vector" $
      \v -> case unit v of
        Nothing -> v == zeroV
        Just u -> near 5 (magnitude v |* u) v
  describe "Rotating Vectors" $ do
    prop "Rotating full turns shouldn't change the vector" $
      \v n -> rotate v (fromIntegral (n :: Int)) `shouldBe` v
    prop "Rotated line of vectors is colinear" $
      \n p t d ->
        let ps = fmap (rotateAround p t . (\i -> p |+ (fromIntegral i |* (d,0)))) [0..(n :: Int)]
        in allColinear ps
  describe "Lines of Vectors" $ do
    prop "Length of line is correct as given" $
      \p gap n -> (n >= 0) ==> length (fromBy p gap n) `shouldBe` n

module Geometry.VectorSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Geometry.Vector
import SpecUtils
import Util.Utils

spec :: Spec
spec = do
  describe "Colinear Vectors" $ do
    prop "If any of three points are equal, then the three are colinear" $
      \p q -> all (colinear 6) [[p,p,q], [p,q,p], [q,p,p], [p,q,q], [q,p,q], [q,q,p], [p,p,p], [q,q,q]]
    prop "Midpoint is colinear with two given vectors" $
      \p q -> colinear 5 [p, q, mid p q]
    prop "Lines have colinear vectors" $
      \p gap -> magnitudeSq gap > 0 ==> colinear 5 $ fromBy p gap 3
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
      \v n -> near 4 (rotate v (fromIntegral (n :: Int))) v -- Only 4 dec nearness because precision is lost in downscaling to unit length
    prop "Rotated line of vectors is colinear" $
      \n p t d ->
        let ps = fmap (rotateAround p t . (\i -> p |+ (fromIntegral i |* (d,0)))) [0..(n :: Int)]
        in colinear 5 ps
  describe "Lines of Vectors" $ do
    prop "Length of line is correct as given" $
      \p gap n -> n >= 0 ==> length (fromBy p gap n) `shouldBe` n

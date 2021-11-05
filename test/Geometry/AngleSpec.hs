module Geometry.AngleSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import SpecUtils
import Geometry.Angle

spec :: Spec
spec = do
  describe "angle stuff" $ do
    prop "getting degrees and converting back" $
      \t -> undegrees (degrees t) `shouldBe` t
    prop "symmetric separation" $
      sym2 separation

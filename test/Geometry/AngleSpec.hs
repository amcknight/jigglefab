module Geometry.AngleSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import SpecUtils
import Geometry.Angle
import Test.QuickCheck
import Utils

spec :: Spec
spec = do
  describe "angle stuff" $ do
    prop "getting degrees and converting back" $ verbose $ 
      \t -> near 5 (undegrees (degrees t)) t
    prop "symmetric separation" $
      sym2 separation

module Geometry.CircleSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import Geometry.Circle
import SpecUtils

spec :: Spec
spec = do
  describe "Getting Circles form points" $ do
    prop "The point should be equal given any order" $
      sym3 circleFrom3

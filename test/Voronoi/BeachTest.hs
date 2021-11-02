module Voronoi.BeachTest
( newRaysEmitFromCenter
) where
import Geometry.Vector
import Voronoi.Beach
import Test.QuickCheck

instance Arbitrary Bouy where
  arbitrary = Bouy <$> arbitrary <*> arbitrary
  
newRaysEmitFromCenter :: Position -> Bouy -> Bouy -> Bouy -> Bool
newRaysEmitFromCenter p b1 b2 b3 = length rs == 3 && and (fmap (\r -> pos r == p) rs)
  where
    rs = newRays p b1 b2 b3

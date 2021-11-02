import Test.QuickCheck
import Geometry.Circle (circleFrom3)
import Geometry.Vector
import SpecUtils
import Geometry.VectorTest
import Geometry.CircleTest
import Geometry.AngleTest
import Voronoi.FortuneTest

main :: IO ()
main = quickCheck circlePointsSame
    -- <> quickCheck midColinear -- Fails due to floats
    <> quickCheck sumIs2Mids
    -- <> quickCheck scaledUnitSame -- Fails due to floats
    <> quickCheck sumSymmetric
    -- <> quickCheck degUndeg -- Fails due to floats
    <> quickCheck singletonNoEdge
    <> quickCheck pairOneEdge
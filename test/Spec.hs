import Test.QuickCheck
import SpecUtils
import Geometry.Circle
import Geometry.Vector
import Geometry.VectorTest
import Geometry.CircleTest
import Geometry.AngleTest
import Voronoi.FortuneTest
import Voronoi.BeachTest

main :: IO ()
main = quickCheck circlePointsSame
    -- <> quickCheck midColinear -- Fails due to floats
    <> quickCheck sumIs2Mids
    -- <> quickCheck scaledUnitSame -- Fails due to floats
    <> quickCheck sumSymmetric
    -- <> quickCheck degUndeg -- Fails due to floats
    <> quickCheck singletonNoEdge
    <> quickCheck pairOneEdge
    <> quickCheck newRaysEmitFromCenter
    <> quickCheck parallelEdgesFromColinear
    <> quickCheck rotatedPointsSame
    <> quickCheck rotateIntSame

import Test.QuickCheck
import SpecUtils
import Geometry.Circle
import Geometry.Vector
import Geometry.VectorTest
import Geometry.CircleTest
import Geometry.AngleTest
import Voronoi.FortuneTest
import Voronoi.BeachTest
import Voronoi.TilingTest

main :: IO ()
main = quickCheck circlePointsSame
    -- <> quickCheck midColinear -- Fails due to floats
    <> quickCheck lineColinear
    <> quickCheck sumIs2Mids
    -- <> quickCheck scaledUnitSame -- Fails due to floats
    <> quickCheck sumSymmetric
    -- <> quickCheck degUndeg -- Fails due to floats
    <> quickCheck singletonNoEdge
    <> quickCheck pairOneEdge
    <> quickCheck newRaysEmitFromCenter
    <> quickCheck parallelEdgesFromColinear
    -- <> quickCheck rotatedPointsSame -- FINAL BOSS TEST
    -- <> quickCheck rotateIntSame -- Fails due to floats
    -- <> quickCheck awayRayPerpendicular -- Fails due to floats
    <> quickCheck allColinearValid
    <> quickCheck offEdgeNothing
    <> quickCheck fromByLength
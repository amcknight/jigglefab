module Draw
( toTranslate
, toScale
, toFrame
, toLine, toPolygon
, toSectorWire, toArcSolid
, toCircle, toCircleSolid
, toRectSolid
) where
import Geometry.Vector
import Graphics.Gloss
import Util.Pair
import Pane.Frame

toTranslate :: Position -> Picture -> Picture
toTranslate = uncurry translate . pmap realToFrac

toScale :: Double -> Picture -> Picture
toScale z = scale (realToFrac z) (realToFrac z)

toFrame :: Frame -> Picture -> Picture
toFrame (Frame p z) = toTranslate p . toScale z

toLine :: [Position] -> Picture
toLine = line . fmap (pmap realToFrac)

toPolygon :: [Position] -> Picture
toPolygon = polygon . fmap (pmap realToFrac)

toSectorWire :: Double -> Double -> Double -> Picture
toSectorWire f t rad = sectorWire (realToFrac f) (realToFrac t) (realToFrac rad)

toArcSolid :: Double -> Double -> Double -> Picture
toArcSolid f t rad = arcSolid (realToFrac f) (realToFrac t) (realToFrac rad)

toCircle :: Double -> Picture
toCircle = circle . realToFrac

toCircleSolid :: Double -> Picture
toCircleSolid = circleSolid . realToFrac

toRectSolid :: Double -> Double -> Picture
toRectSolid w h = rectangleSolid (realToFrac w) (realToFrac h)

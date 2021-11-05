module Geometry.AngleTest
( degUndeg
) where

import SpecUtils
import Geometry.Vector
import Geometry.Angle

degUndeg :: Turn -> Bool
degUndeg t = undegrees (degrees t) == t

sepSym :: Turn -> Turn -> Bool
sepSym = sym2 separation

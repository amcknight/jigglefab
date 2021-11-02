module Geometry.AngleTest
( degUndeg
) where

import Geometry.Angle
import SpecUtils

degUndeg :: Turn -> Bool
degUndeg t = undegrees (degrees t) == t

sepSym :: Turn -> Turn -> Bool
sepSym = sym2 separation

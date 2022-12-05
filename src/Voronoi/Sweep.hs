{-# LANGUAGE LambdaCase #-}
module Voronoi.Sweep
( Sweep(..)
, antiSweeps
) where

import Data.List (partition)
import Geometry.Angle

data Sweep = Sweep Turn Turn | FullSweep deriving Show

trivial :: Sweep -> Bool
trivial (Sweep x y) = x == y
trivial FullSweep = False

antiSweeps :: [Sweep] -> [Sweep]
antiSweeps ss = filter (not . trivial) $ case crossSweep of
  Nothing -> antiSweeps' 0 1 ss
  Just (Sweep end start) -> antiSweeps' start end nonCrossSweeps
  Just _ -> error "Impossible non-sweep cross sweep"
  where
    (crossSweeps, nonCrossSweeps) = partition (\case (Sweep f t) -> f > t; FullSweep -> False) ss
    crossSweep = case crossSweeps of
      [] -> Nothing
      [s] -> Just s
      css -> Just (head css) -- TODO: error "More than one Sweep crossed 1"

antiSweeps' :: Turn -> Turn -> [Sweep] -> [Sweep]
antiSweeps' _ _ (FullSweep:_) = []
antiSweeps' s e [] = [Sweep s e]
antiSweeps' s e ((Sweep f t):ss)
  | s > f = antiSweeps' t e ss -- TODO: error "Sweeps overlap"
  | otherwise = Sweep s f : antiSweeps' t e ss

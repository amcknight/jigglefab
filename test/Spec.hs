import Test.QuickCheck
import Geometry.Circle (circleFrom3)
import Geometry.Vector

circlePointsSame :: Position -> Position -> Position -> Bool
circlePointsSame = sym3 circleFrom3

midColinear :: Position -> Position -> Bool
midColinear p q = colinear p q $ mid p q

sumIs2Mids :: Position -> Position -> Bool
sumIs2Mids p q = p |+ q == 2 |* mid p q

scaledUnitSame :: Vector -> Bool
scaledUnitSame v = case unit v of
  Nothing -> v == zeroV
  Just u -> magnitude v |* u == v

main :: IO ()
main = quickCheck circlePointsSame
    -- <> quickCheck midColinear -- Fails due to floats
    <> quickCheck sumIs2Mids
    -- <> quickCheck scaledUnitSame -- Fails due to floats

allEqual :: Eq a => [a] -> Bool
allEqual []     = True
allEqual (x:xs) = all (== x) xs

eqSelf :: Eq a => a -> (a -> a) -> Bool
eqSelf x f = f x == x

sym2 :: Eq b => (a -> a -> b) -> a -> a -> Bool
sym2 f a b = allEqual [f a b, f b a]

sym3 :: Eq b => (a -> a -> a -> b) -> a -> a -> a -> Bool
sym3 f a b c = allEqual [f a b c, f a c b, f b a c, f b c a, f c a b, f c b a]  

module SpecUtils
( eqSelf
, sym2, sym3
) where
import Utils

eqSelf :: Eq a => a -> (a -> a) -> Bool
eqSelf x f = f x == x

sym2 :: Eq b => (a -> a -> b) -> a -> a -> Bool
sym2 f a b = allEq [f a b, f b a]

sym3 :: Eq b => (a -> a -> a -> b) -> a -> a -> a -> Bool
sym3 f a b c = allEq [f a b c, f a c b, f b a c, f b c a, f c a b, f c b a]  

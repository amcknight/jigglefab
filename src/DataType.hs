module DataType
( Type(..), Con(..), Token(..)
, topCon, conNames, con, getCon, subcons
) where
import Debug.Trace

data Con = Con { name :: String, ts :: [Type] }
newtype Type = Type { cs :: [Con] }
type Token = [String]

-- TODO: show these datatypes better
instance Show Con where
  show c = unwords $ ["Con", name c] ++ fmap show (ts c)
instance Show Type where
  show t = unwords $ "Type" : fmap show (cs t)

topCon :: Type -> Con
topCon ty = Con "" [ty]

isTopCon :: Con -> Bool
isTopCon (Con "" [_]) = True
isTopCon _ = False

conNames :: Con -> [String]
conNames c = fmap name $ concatMap cs $ ts c

con :: Con -> String -> Maybe (Int, Con)
con c = con' (concatMap cs (ts c)) 0
con' :: [Con] -> Int -> String -> Maybe (Int, Con)
con' [] _ n' = Nothing
con' (c@(Con n _) : cs) i n' = if n == n'
  then Just (i, c)
  else con' cs (i+1) n'

getCon :: Con -> Token -> Maybe Con
getCon c [] = Just c
getCon c (n:ns) = case con c n of
  Nothing -> Nothing
  Just (_, scon) -> getCon scon ns

subcons :: Con -> [Con]
subcons (Con _ tys) = concatMap cs tys

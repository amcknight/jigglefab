module DataType
( Con(..), Token(..), TkPart(..)
, conName
, topCon
, toToken, toTkPart
, extendTkPart, reduceTkPart
, numNames
, firstHole
) where

import Data.Maybe (isJust)

data Con = Con0 String | Con1 String Type | Con2 String Type Type deriving Show
type Type = [Con]
data Token = Tk0 String | Tk1 String Token | Tk2 String Token Token deriving Show
data TkPart = H | V String | Z String | O String TkPart | T String TkPart TkPart

instance Show TkPart where
  show H = "_"
  show (V s) = "_" ++ s
  show (Z s) = s
  show (O s t) = "(" ++ unwords [s, show t] ++ ")"
  show (T s t1 t2) = "(" ++ unwords [s, show t1, show t2] ++ ")"

conName :: Con -> String
conName (Con0 n) = n
conName (Con1 n _) = n
conName (Con2 n _ _) = n

con :: Type -> String -> Con
con cs n = case filter ((== n) . conName) cs of
  [] -> error "Should only find one Con. Found none."
  [c] -> c
  _ -> error "Should only find one Con. Found more."

topCon :: Type -> Con
topCon = Con1 ""

toToken :: TkPart -> Maybe Token
toToken tkp = case tkp of
  H -> Nothing
  V s -> Nothing
  Z s -> Just $ Tk0 s
  O s tp -> case toToken tp of
    Nothing -> Nothing
    Just t -> Just $ Tk1 s t
  T s tp1 tp2 -> case toToken tp1 of
    Nothing -> Nothing
    Just t1 -> case toToken tp2 of
      Nothing -> Nothing
      Just t2 -> Just $ Tk2 s t1 t2

toTkPart :: Token -> TkPart
toTkPart (Tk0 s) = Z s
toTkPart (Tk1 s t) = O s $ toTkPart t
toTkPart (Tk2 s t1 t2) = T s (toTkPart t1) (toTkPart t2)

extendTkPart :: Con -> TkPart -> Maybe TkPart
extendTkPart c tkp = case tkp of
  H -> Just $ toHole c
  V _ -> Just $ toHole c
  Z _ -> Nothing
  O s subTkp -> case c of
    Con1 _ ty -> (Just . O s) =<< extendTkPart (con ty s) subTkp
    _ -> error "Type error: TkPart and Con don't match (1)"
  T s subTkp1 subTkp2 -> case c of
    Con2 _ ty1 ty2 -> case extendTkPart (con ty1 s) subTkp1 of
      Nothing -> case extendTkPart (con ty2 s) subTkp2 of
        Nothing -> Nothing
        Just newTkp -> Just $ T s subTkp1 newTkp
      Just newTkp -> Just $ T s newTkp subTkp2
    _ -> error "Type error: TkPart and Con don't match (2)"

reduceTkPart :: TkPart -> Maybe TkPart
reduceTkPart H = Nothing
reduceTkPart (V s) = Nothing
reduceTkPart (Z _) = Just H
reduceTkPart (O s t) = case reduceTkPart t of
  Nothing -> Just H
  Just tp -> Just tp
reduceTkPart (T s t1 t2) = case reduceTkPart t2 of
  Nothing -> case reduceTkPart t1 of
    Nothing -> Just H
    Just tp -> Just tp
  Just tp -> Just tp

toHole :: Con -> TkPart
toHole (Con0 s) = Z s
toHole (Con1 s _) = O s H
toHole (Con2 s _ _) = T s H H

firstHole :: Con -> TkPart -> Maybe Con
firstHole c tkp = case tkp of
  H -> Just c
  V _ -> Just c
  Z _ -> Nothing
  O s subTkp -> case c of
    Con1 _ ty -> firstHole (con ty s) subTkp
    _' -> error "Type error: TkPart and Con don't match (1)"
  T s subTkp1 subTkp2 -> case c of
    Con2 _ ty1 ty2 -> case firstHole (con ty1 s) subTkp1 of
      Nothing -> firstHole (con ty2 s) subTkp2
      Just con -> Just con
    _ -> error "Type error: TkPart and Con don't match (2)"

numNames :: TkPart -> Int
numNames tkp = case tkp of
  H -> 0
  V _ -> 0
  Z _ -> 1
  O _ subTkp -> 1 + numNames subTkp 
  T _ subTkp1 subTkp2 -> 1 + numNames subTkp1 + numNames subTkp2

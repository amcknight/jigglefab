module Env
( Env (Env)
, radius
, model
) where
import Space
import Model

data Env = Env Radius Model

radius :: Env -> Radius
radius (Env rad _) = rad

model :: Env -> Model
model (Env _ m) = m

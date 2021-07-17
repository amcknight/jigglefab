module Env
( Env (Env)
, radius
, model
) where
import Space
import Model

data Env = Env
  { radius :: Radius
  , model :: Model
  }

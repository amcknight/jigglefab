module Models
( Models
) where

import Model

type Models = (Model, Model)

factorAt :: Int -> Model -> Models
factorAt = splitAt

-- Ignores the fact that radiuses could be incompatible
join :: Model -> Model -> Model
join = (++)

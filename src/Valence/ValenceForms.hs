module Valence.ValenceForms
( randomForm
) where

import Utils
import Form
import Valence.Valence
import System.Random
import Control.Monad.State
import Vector
import Ball
import Point

randomForm :: Float -> Float -> Int -> R (Form Valence)
randomForm _ _ 0 = do pure mempty
randomForm speed size num = do
  seed <- get
  let (valence, pSeed) = randomR (1, 3) seed
  put pSeed
  pos <- randomVIn size
  vel <- randomV speed
  let headForm = ballForm (Ball (Point pos vel) (vale valence))
  nextForms <- randomForm size speed (num-1)
  pure $ headForm <> nextForms

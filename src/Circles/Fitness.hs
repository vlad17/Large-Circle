-- Fitness module contains circle ranking function.

module Circles.Fitness where

import Utils ((>|>))

import qualified Circles.Circles as Circles
import qualified Circles.CirclesArena as CirclesArena

-- circleFitness circle circles w h
-- Returns a non-negative fitness score for 'circle' given the circles
-- it shouldn't intersect with, 'circles', and the range it should be in
-- completely.
circleFitness :: Circles.Circle -> CirclesArena.CirclesArena 
  -> Int -> Int -> Double

-- Implementation

circleFitness circle circles w h  =
  -- TODO try intersecting area (somehow...), distance from center of image.
  (if not inRange || CirclesArena.intersects circles circle then 0 else r)
  >|> fromIntegral
  where
    r = Circles.radius circle
    inRange = let (x, y, _) = Circles.toTuple circle
              in x - r >= 0 && x + r <= w && y - r >= 0 && y + r <= h

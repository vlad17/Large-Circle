-- Fitness module contains circle ranking function.

module Circles.Fitness where

import qualified Circles.Circles as Circles
import qualified Data.List as List

-- circleFitness circle circles w h
-- Returns a non-negative fitness score for 'circle' given the circles
-- it shouldn't intersect with, 'circles', and the range it should be in
-- completely.
circleFitness :: Circles.Circle -> [Circles.Circle] -> Int -> Int -> Double

-- Implementation

circleFitness circle circles w h =
  if not inRange && List.any (intersect circle) circles then 1
  else fromIntegral $ 1 + Circles.radius circle
  where
    inRange = let (x, y, r) = Circles.toTuple circle
              in x - r > 0 && x + r < w && y - r > 0 && y + r < h
    intersect c1 c2 =
      let (x1, y1, r1) = Circles.toTuple c1
          (x2, y2, r2) = Circles.toTuple c2
          square x = x * x
      in square (x1 - x2) + square (y1 - y2) > square (r1 + r2)

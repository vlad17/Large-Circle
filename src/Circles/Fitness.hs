-- Fitness module contains circle ranking function.

module Circles.Fitness where

import qualified Circles.Circles as Circles
import qualified Data.List as List

-- circleFitness circle circles
-- Returns a non-negative fitness score for 'circle' given the circles
-- it shouldn't intersect with 'circles'
circleFitness :: Circles.Circle -> [Circles.Circle] -> Double

-- Implementation

circleFitness circle circles =
  if List.any (intersect circle) circles then 1
  else fromIntegral $ 1 + Circles.radius circle
  where
    intersect c1 c2 =
      let (x1, y1, r1) = Circles.toTuple c1
          (x2, y2, r2) = Circles.toTuple c2
          square x = x * x
      in square (x1 - x2) + square (y1 - y2) > square (r1 + r2)

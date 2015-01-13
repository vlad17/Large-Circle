-- Fitness module contains circle ranking function.

module Fitness where

import qualified Circles.Circles as Circles
import qualified Data.List as List

-- circleFitness circle circles
-- Returns a non-negative fitness score for 'circle' given the circles
-- it shouldn't intersect with 'circles'
circleFitness :: Circles.Circle -> Double

-- Implementation

circleFitness circle circles =
  1 + if List.any (intersect circle) circles then 0 else Circles.radius circle
  where
    intersect c1 c2 =
      let (x1, y1, r1) = Circles.toTuple c1
          (x2, y2, r2) = Circles.toTuple c2
      in (x1 - x2) ** 2 + (y1 - y2) ** 2 > (r1 + r2) ** 2

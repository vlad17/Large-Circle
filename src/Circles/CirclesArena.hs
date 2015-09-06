-- A CirclesArena is a structure that takes care of methods pertaining to
-- the interaction (i.e., potential intersection, etc.) of one modifyable
-- circle with a set of static circles.
module Circles.CirclesArena where

import qualified Circles.Circles as Circles
import Utils (square, isqrt)

import qualified Data.List as List

-- TODO: perhaps change this to a smarter data structure (e.g. a quadtree or something).

type CirclesArena = [Circles.Circle]

-- intersects circles circle
-- Returns whether 'circle' intersects with any of the circles in 'circles'.
intersects :: CirclesArena -> Circles.Circle -> Bool

-- maxPossibleRadius circles x y 
-- Returns the largest possible circle centered at (x, y) that doesn't
-- intersect with any of 'circles' if one exists.
maxPossibleRadius :: CirclesArena -> Int -> Int -> Maybe Int

-- Implementation

intersects circles circle = List.any (intersect circle) circles
  where
    intersect c1 c2 =
      let (x1, y1, r1) = Circles.toTuple c1
          (x2, y2, r2) = Circles.toTuple c2
      in square (x1 - x2) + square (y1 - y2) < square (r1 + r2)

maxPossibleRadius circles x y =
  fmap minimum $ sequence $ map largestRadius circles
  where
    freeRoom circle = let (x', y', r) = Circles.toTuple circle in
      isqrt (square (x - x') + square (y - y')) - r
    largestRadius circle = let rsq = freeRoom circle in
      if rsq < 0 then Nothing else Just rsq

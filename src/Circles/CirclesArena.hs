-- A CirclesArena is a structure that takes care of methods pertaining to
-- the interaction (i.e., potential intersection, etc.) of one modifyable
-- circle with a set of static circles.
module Circles.CirclesArena (CirclesArena, intersects) where

import qualified Circles.Circles as Circles
import qualified Data.List as List

type CirclesArena = [Circles.Circle]

intersects :: CirclesArena -> Circles.Circle -> Bool
intersects circles circle = List.any (intersect circle) circles
  where
    intersect c1 c2 =
      let (x1, y1, r1) = Circles.toTuple c1
          (x2, y2, r2) = Circles.toTuple c2
          square x = x * x
      in square (x1 - x2) + square (y1 - y2) < square (r1 + r2)


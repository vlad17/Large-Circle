module Circles.Circles where

import System.Random as Random

data Circle = Circle { x :: Int, y :: Int, r :: Int }

-- makeRandom n w h minr maxr
-- Makes n random circles with centers in the rectangle (0, 0) x (w, h)
-- with radii uniformly selected in the range (minr, maxr)
makeRandom :: Int -> Int -> Int -> Int -> Int -> IO [Circle]

-- Implementation

makeRandom n w h minr maxr = sequence $ replicate n randomCircle
  where randomCircle = do
          xx <- Random.randomRIO (0, w)
          yy <- Random.randomRIO (0, h)
          rr <- Random.randomRIO (minr, maxr)
          return Circle { x = xx, y = yy, r = rr }

module Circles.Circles where

import System.Random as Random

data Circle = Circle { xCoord :: Int, yCoord :: Int, radius :: Int }
  deriving (Eq)

-- toTuple circle
-- Returns the tuple (x, y, r) for the circle.
toTuple :: Circle -> (Int, Int, Int)
toTuple (Circle { xCoord = x, yCoord = y, radius = r }) = (x, y, r)

-- makeRandom n w h minr maxr
-- Makes n random circles with centers in the rectangle (0, 0) x (w, h)
-- with radii uniformly selected in the range (minr, maxr).
makeRandom :: Int -> Int -> Int -> Int -> Int -> IO [Circle]

-- maxR w h
-- Maximum radius for a circle in the given screen
maxR :: Int -> Int -> Int

-- Implementation

makeRandom n w h minr maxr = sequence $ replicate n randomCircle
  where randomCircle = do
          x <- Random.randomRIO (0, w)
          y <- Random.randomRIO (0, h)
          r <- Random.randomRIO (minr, maxr)
          return $ Circle x y r

maxR w h = min w h `quot` 2
instance Show Circle where
  show (Circle { xCoord = x, yCoord = y, radius = r }) =
    "Circle " ++ show r ++ " (" ++ show x ++ ", " ++ show y ++ ")"

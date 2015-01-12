-- Main file
-- Generates a window with randomly placed black circles.
-- Then draws a red circle which continues expancing in the window
-- until it reaches the edges.

import qualified Circles.Circles as Circles
import qualified CircleGUI.CircleGUI as CircleGUI
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Data.IORef as IORef
import qualified Genetic.Learning as Learning
--import qualified System.Random as Random

-- Asyc "circle enlargment" task which simulates an animation
-- as a calculation generates a new circle.
enlargeCircle :: IO () -> IORef.IORef Circles.Circle -> Int -> IO ()
enlargeCircle update circle maxRadius =
  let
    increment = max 1 $ maxRadius `div` 100
    enlarge = do
      (x, y, r) <- IORef.readIORef circle >>= return . Circles.toTuple
      Monad.when (r < maxRadius) $
        IORef.writeIORef circle $ Circles.Circle x y $ r + increment
      return r
    loop = do
      r <- enlarge
      putStrLn $ "radius " ++ show r
      update
      Concurrent.threadDelay 100000
      Monad.when (r < maxRadius) loop
  in loop

main :: IO ()
main = do
  -- Generate the window and retreive its sizes
  window <- CircleGUI.start "Circles"
  (w, h) <- CircleGUI.size window

  -- Generate the circles for the given window
  let maxRadius = flip div 4 $ min w h
  randomCircles <- Circles.makeRandom 15 w h (maxRadius `div` 4) maxRadius

  -- Put red circle in the center, then add the circles to a canvas
  -- and retrieve the mutable redCircle updater.
  redCircle <- IORef.newIORef $ Circles.Circle (w `div` 2) (h `div` 2) 0
  update <- CircleGUI.addCircles window randomCircles redCircle

  -- Set a thread to make the redCircle larger as time goes on.
  CircleGUI.postDisplay window $ Monad.void . Concurrent.forkIO $
    enlargeCircle update redCircle $ maxRadius * 2

  -- Add them to the canvas and display
  CircleGUI.finish window

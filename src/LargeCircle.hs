-- Main file (currently does nothing)

import qualified Circles.Circles as Circles
import qualified CircleGUI.CircleGUI as CircleGUI
import qualified Control.Concurrent as Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Control.Monad as Monad

import qualified Graphics.UI.Gtk as Gtk

-- Asyc "circle enlargment" task which simulates an animation
-- as a calculation generates a new circle.
enlargeCircle :: (Int -> Int -> Int -> Int -> IO ()) ->
                 STM.TVar Circles.Circle -> Int -> IO ()
enlargeCircle update circle maxRadius =
  let
    increment = max 1 $ maxRadius `div` 100
    enlarge = do
      (x, y, r) <- STM.readTVar circle >>= return . Circles.toTuple
      if r >= maxRadius then return () else
        STM.writeTVar circle $ Circles.Circle x y $ r + increment
    loop = do
      Concurrent.threadDelay 100000
      (x, y, r) <- STM.atomically $ STM.readTVar circle >>= return . Circles.toTuple
      Gtk.postGUISync $ update (x - r - 5) (y - r - 5) (2 * r + 10) (2 * r + 10)
      putStrLn $ "radius " ++ show r
      STM.atomically enlarge
      loop
  in loop

main :: IO ()
main = do
  -- Generate the window and retreive its sizes
  window <- CircleGUI.start "Circles"
  (w, h) <- CircleGUI.size window

  -- Generate the circles for the given window and add them to the canvas
  let maxRadius = flip div 4 $ min w h
  randomCircles <- Circles.makeRandom 15 w h (maxRadius `div` 4) maxRadius
  let chosenCircle = Circles.Circle (w `div` 2) (h `div` 2) 0
  redCircle <- STM.atomically $ STM.newTVar chosenCircle
  update <- CircleGUI.addCircles window randomCircles redCircle

  -- Set a thread to make the redCircle larger as time goes on.
  Gtk.onRealize window $ Monad.void . Concurrent.forkIO $ enlargeCircle update redCircle $ maxRadius * 2

  -- Add them to the canvas and display
  CircleGUI.finish window

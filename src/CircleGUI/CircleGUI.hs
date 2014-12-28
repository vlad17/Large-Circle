-- Thin layer between circle application and GTK for a single-window
-- application

module CircleGUI.CircleGUI where

import qualified Circles.Circles as Circles
import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.Trans as Trans
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Gdk.GC as Gtk

type Window = Gtk.Window

-- start title
-- Must be called exactly once, before any other commands.
-- Makes a window with the title 'title' centered on the screen,
-- with dimensions equal to half the screen size in both directions.
start :: String -> IO Window

-- finish window
-- Must be called exactly once, after all other commands.
-- Takes current thread to be main GUI thread.
finish :: Window -> IO ()

-- size window
-- Returns window's (width, height).
size :: Window -> IO (Int, Int)

-- addCircles window circles circle
-- Adds a canvas with the static circles drawn to the window in black.
-- The canvas will have the circle pointed to by 'circle' drawn in red
-- since the pointer is mutable, it will be updated.
addCircles :: Window -> [Circles.Circle] -> STM.TVar Circles.Circle
              -> IO (Int -> Int -> Int -> Int -> IO ())

-- Implementation

start title = do
  -- Initialize GUI toolkit
  _ <- Gtk.initGUI

  -- Generate a new window with the default specifications and return it
  window <- Gtk.windowNew
  Gtk.windowSetTitle window title
  Gtk.windowSetPosition window Gtk.WinPosCenter
  sw <- Gtk.screenWidth
  sh <- Gtk.screenHeight
  Gtk.windowSetDefaultSize window (sw `div` 2) (sh `div` 2)
  return window

finish window = do
  -- Register exit handler, ignore the ConnectId (we won't ever unregister it)
  _ <- Gtk.onDestroy window Gtk.mainQuit

  -- Show the window and loop current thread, waiting for input.
  Gtk.widgetShowAll window
  Gtk.mainGUI

size = Gtk.windowGetDefaultSize

addCircles window circles circle = do
  -- Generate a canvas that takes up the entire window
  (w, h) <- size window
  canvas <- Gtk.drawingAreaNew
  _ <- Gtk.onSizeRequest canvas $ return (Gtk.Requisition w h)

  -- Set the repaint handler and add to the window
  _ <- Gtk.onExpose canvas $ updateCanvas canvas
  Gtk.containerAdd window canvas
  return $ \ x y w h -> (Gtk.widgetGetDrawWindow canvas >>= (\ wi -> Gtk.drawWindowInvalidateRect wi (Gtk.Rectangle x y w h) True))
  where
    drawCircle drawWindow drawContext circ =
      let (x, y, r) = Circles.toTuple circ
          (cx, cy, rr) = (x - r, y - r, r * 2)
      in Gtk.drawArc drawWindow drawContext False cx cy rr rr 0 (64 * 360)
    updateCanvas canvas _ = do
      drawWindow <- Gtk.widgetGetDrawWindow canvas
      blackAndThin <- Gtk.gcNew drawWindow
      mapM_ (drawCircle drawWindow blackAndThin) circles
      redAndThin <- Gtk.gcNewWithValues drawWindow $
                    Gtk.newGCValues { Gtk.foreground = Gtk.Color 65535 0 0 }
      circleSnapshot <- STM.atomically $ STM.readTVar circle
      drawCircle drawWindow redAndThin circleSnapshot
      return True

-- Thin layer between circle application and GTK for a single-window
-- application

module CircleGUI.CircleGUI where

import qualified Circles.Circles as Circles
import qualified Control.Monad as Monad
import qualified Data.IORef as IORef
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

-- postDisplay window action
-- Registers action to be done after window has been displayed.
postDisplay :: Window -> IO () -> IO ()

-- addCircles window circles circle
-- Adds a canvas with the static circles drawn to the window in black.
-- The canvas will have the circle pointed to by 'circle' drawn in red
-- since the pointer is mutable, it will be updated.
--
-- After making a mutation to the parameter circle transaction variable,
-- make sure to call the update handler. This is safe to do on any thread
-- (it notifies the main event dispatch loop asynchronously).
--
-- The update function should only be called from one thread at a time,
-- by the same thread that mutates the parameter circle reference.
--
-- Note that 'update' can only be called after the window is displayed;
-- in other words, after calling 'finish'. Thus a method which updates the
-- variable should do so on another thread which is started by a call
-- to 'postDisplay'
addCircles :: Window -> [Circles.Circle] -> IORef.IORef Circles.Circle
              -> IO (IO ())

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

  -- Create the update function. First, generate a reference to a
  -- "history," the last circle that was drawn (its region needs to be
  -- invalidated because it no longer exists there).
  history <- IORef.readIORef circle >>= IORef.newIORef
  return $ invalidateMutableCircle history canvas
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
      circleSnapshot <- IORef.readIORef circle
      drawCircle drawWindow redAndThin circleSnapshot
      return True
    invalidateMutableCircle history canvas = Gtk.postGUIAsync $ do
      current <- IORef.readIORef circle
      prev <- IORef.readIORef history
      Monad.unless (current == prev) $ do
        drawWindow <- Gtk.widgetGetDrawWindow canvas
        let invalidateRegions = do
              Gtk.drawWindowInvalidateRect drawWindow (bounding prev) True
              Gtk.drawWindowInvalidateRect drawWindow (bounding current) True
        Gtk.postGUIAsync invalidateRegions
        IORef.writeIORef history current
    bounding circ =
      let (x, y, r) = Circles.toTuple circ
          buffer = 5
          br = r + buffer
      in Gtk.Rectangle (x - br) (y - br) (2 * br) (2 * br)

postDisplay window = Monad.void . Gtk.onRealize window

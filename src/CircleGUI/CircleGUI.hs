-- Thin layer between circle application and GTK for a single-window
-- application

module CircleGUI.CircleGUI where

import Control.Monad ((>>))

import qualified Circles.Circles as Circles
import qualified Data.Tuple.Sequence as TupleSeq
import qualified Control.Monad.Trans as Trans
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Gdk.GC as Gtk

type Window = Gtk.Window
type DrawingArea = Gtk.DrawingArea

-- start title
-- Must be called exactly once, before any other commands.
-- Makes a window with the title 'title' centered on the screen.
start :: String -> IO Window

-- finish window
-- Must be called exactly once, after all other command.
-- Takes current thread to be main GUI thread.
finish :: Window -> IO ()

-- getScreenSize
-- Returns screen (width, height)
getScreenSize :: IO (Int, Int)

-- setDefaultSize window w h
-- Sets window to have (width, height) be (w, h)
setDefaultSize :: Window -> Int -> Int -> IO ()

-- addCanvas window circles circle w h
-- Adds a canvas with the static circles drawn to the window and returns
-- the canvas. Canvase has dimensions (w, h). The list of circles is drawn
-- in black while the single circle is drawn in red.
addCanvas :: Window -> [Circles.Circle] -> Circles.Circle
             -> Int -> Int -> IO DrawingArea

-- Implementation

start title = do
  -- Initialize GUI toolkit
  _ <- Gtk.initGUI

  -- Generate a new window with the default specifications and return it
  window <- Gtk.windowNew
  Gtk.windowSetTitle window title
  Gtk.windowSetPosition window Gtk.WinPosCenter
  return window

finish window = do
  -- Register exit handler, ignore the ConnectId (we won't ever unregister it)
  _ <- Gtk.onDestroy window Gtk.mainQuit

  -- Show the window and loop current thread, waiting for input.
  Gtk.widgetShowAll window
  Gtk.mainGUI

getScreenSize = TupleSeq.sequenceT (Gtk.screenWidth, Gtk.screenHeight)

setDefaultSize = Gtk.windowSetDefaultSize

addCanvas window circles circle w h = do
  canvas <- Gtk.drawingAreaNew
  _ <- Gtk.onSizeRequest canvas $ return (Gtk.Requisition w h)
  _ <- Gtk.onExpose canvas $ drawCanvas canvas
  Gtk.containerAdd window canvas
  return canvas
  where
    drawCircle drawWindow drawContext
      (Circles.Circle { Circles.x = x, Circles.y = y, Circles.r = r }) =
        let (cx, cy, rr) = (x - r, y - r, r * 2)
        in Gtk.drawArc drawWindow drawContext False cx cy rr rr 0 (64 * 360)
    drawCanvas canvas _ = do
      drawWindow <- Gtk.widgetGetDrawWindow canvas
      blackAndThin <- Gtk.gcNew drawWindow
      mapM_ (drawCircle drawWindow blackAndThin) circles
      redAndThin <- Gtk.gcNewWithValues drawWindow $
                    Gtk.newGCValues { Gtk.foreground = Gtk.Color 65535 0 0 }
      drawCircle drawWindow redAndThin circle
      return True

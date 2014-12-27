-- Main file (currently does nothing)

import qualified Circles.Circles as Circles
import qualified CircleGUI.CircleGUI as CircleGUI

main :: IO ()
main = do
  -- Generate the window
  window <- CircleGUI.start "Circles"

  -- Request the screen size, set window to be half that in both dimensions.
  (sw, sh) <- CircleGUI.getScreenSize
  let (w, h) = (sw `div` 2, sh `div` 2)
  CircleGUI.setDefaultSize window w h

  let maxRadius = flip div 4 $ min w h
  randomCircles <- Circles.makeRandom 15 w h (maxRadius `div` 4) maxRadius
  _ <- CircleGUI.addCanvas window randomCircles w h

  -- Show it!
  CircleGUI.finish window

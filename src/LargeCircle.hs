-- Main file
-- Generates a window with randomly placed black circles.
-- Then draws a red circle according to what the genetic algorithm
-- defines is the best location that makes the circle as large as
-- possible without intersecting any of the black circles.

import qualified Circles.Circles as Circles
import qualified Circles.Encoding as Encoding
import qualified Circles.Fitness as Fitness
import qualified CircleGUI.CircleGUI as CircleGUI
import qualified Control.Concurrent as Concurrent
import qualified Control.Monad as Monad
import qualified Data.Array.Unboxed as Array
import qualified Data.IORef as IORef
import qualified Genetic.Learning as Learning
import qualified System.Random as Random

-- runFindCircle update circle fit
--
-- Uses the 'update' update handler to let the GUI thread know
-- when the reference to the 'circle' has changed, which occurs every
-- few hundred milliseconds, with a new generation's solution to the
-- single circle-packing problem.
--
-- Uses fitness function 'fit'
--
-- Call does not exit.
runFindCircle ::
  IO ()
  -> IORef.IORef Circles.Circle
  -> (Learning.Chromosome -> Double)
  -> IO ()
runFindCircle update circle fit =
  let
    loop learner = do
      let best = Encoding.decode . Array.elems $ Learning.getBest learner
      IORef.writeIORef circle best
      putStrLn $ "generation " ++ show (Learning.getGen learner)
        ++ ", " ++ show best
      update
      Concurrent.threadDelay 100000 -- TODO incorporate computation time here
      loop $ Learning.learn learner
    cross = 0.7
    mut = 0.001
    len = Encoding.codeSize
    num = 1000
    initialLearner rgen = Learning.create rgen fit cross mut len num
  in Random.randomIO >>= loop . initialLearner . Random.mkStdGen

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

  -- Set a thread to make the redCircle closer to the circle-packing
  -- solution as time goes on, but only after GTK+ lets updates occur.
  let fit = (\ circ -> Fitness.circleFitness circ randomCircles w h)
            .  Encoding.decode . Array.elems
  CircleGUI.postDisplay window $ Monad.void . Concurrent.forkIO $
    runFindCircle update redCircle fit

  -- Add them to the canvas and display
  CircleGUI.finish window

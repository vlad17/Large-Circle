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

-- runFindCircle update circle fit decoder w h
--
-- Uses the 'update' update handler to let the GUI thread know
-- when the reference to the 'circle' has changed, which occurs every
-- few hundred milliseconds, with a new generation's solution to the
-- single circle-packing problem.
--
-- Uses 'decoder' for decoding chromosomes
--
-- Assumes canvas size is w x h
--
-- Uses fitness function 'fit'
--
-- Call does not exit.
runFindCircle ::
  IO ()
  -> IORef.IORef Circles.Circle
  -> (Learning.Chromosome -> Double)
  -> (Learning.Chromosome -> Circles.Circle)
  -> Int
  -> Int
  -> IO ()
-- TODO clean up parameters here (pass normal fitness)
runFindCircle update circle fit decoder w h =
  let
    loop learner = do
      let best = Learning.getBest learner
          bestCircle = decoder best
      IORef.writeIORef circle bestCircle
      putStrLn $ "generation " ++ show (Learning.getGen learner)
        ++ ", " ++ show bestCircle ++ ": " ++ show (fit best)
      update
      Concurrent.threadDelay 100000 -- TODO incorporate computation time here
      loop $ Learning.learn learner
    cross = 0.7
    mut = 0.01
    len = Encoding.codeSize w h
    num = 1000
    initialLearner rgen = Learning.create rgen fit cross mut len num
  in do
    seed <- Random.randomIO
    putStrLn $ "Seed: " ++ show seed
    loop . initialLearner $ Random.mkStdGen seed

main :: IO ()
main = do
  -- Generate the window and retreive its sizes
  window <- CircleGUI.start "Circles"
  (w, h) <- CircleGUI.size window
  putStrLn $ "Window size: " ++ show w ++ "x" ++ show h

  -- Generate the circles for the given window
  let maxRadius = flip div 4 $ min w h
  randomCircles <- Circles.makeRandom 15 w h (maxRadius `div` 4) maxRadius

  -- Put red circle in the center, then add the circles to a canvas
  -- and retrieve the mutable redCircle updater.
  redCircle <- IORef.newIORef $ Circles.Circle (w `div` 2) (h `div` 2) 0
  update <- CircleGUI.addCircles window randomCircles redCircle

  -- Set a thread to make the redCircle closer to the circle-packing
  -- solution as time goes on, but only after GTK+ lets updates occur.
  let decoder = Encoding.decoder w h . Array.elems
      fit = (\ circ -> Fitness.circleFitness circ randomCircles w h) . decoder
  CircleGUI.postDisplay window $ Monad.void . Concurrent.forkIO $
    runFindCircle update redCircle fit decoder w h

  -- Add them to the canvas and display
  CircleGUI.finish window

-- Module that handles sampling from distributions

module Genetic.Sampler where

import qualified Control.Monad as Monad
import qualified Control.Monad.Random as Random

-- TODO filter interface.

type SamplingDistribution a = [a]
-- TODO sorted array

-- makeAssoc vals weights
-- Makes a sampling distribution where the values 'vals' take on the
-- weights 'weights'. The normalized weights become the probability of
-- selection for the parameter values, after normalization. Weights
-- should be nonnegative and there should be as many in the list as there
-- are values (nonininfinite).
makeAssoc :: [a] -> [Double] -> SamplingDistribution a
makeAssoc vals weights = []

-- sample dist
-- samples the distribution 'dist'
sample :: (Monad.Monad m, Random.RandomGen g) =>
          SamplingDistribution a -> Random.RandT g m a
sample dist = return $ dist !! 0 -- TODO binary search

-- shouldI prob
-- Returns whether a Bernoulli trial conducted by the parameter random
-- number generator was successful, with probability 'prob'.
shouldI :: (Monad.Monad m, Random.RandomGen g) =>
           Double -> Random.RandT g m Bool

shouldI prob = Random.getRandomR (0, 1 :: Double) >>= return . (< prob)

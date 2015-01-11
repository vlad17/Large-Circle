-- Module that handles sampling from distributions

module Genetic.Sampler where

import qualified System.Random as Random

-- TODO filter interface.

type SamplingDistribution a = [a]
-- todo sorted array

-- makeAssoc vals weights
-- Makes a sampling distribution where the values 'vals' take on the
-- weights 'weights'. The normalized weights become the probability of
-- selection for the parameter values, after normalization. Weights
-- should be nonnegative and there should be as many in the list as there
-- are values (nonininfinite).
makeAssoc :: [a] -> [Double] -> SamplingDistribution a
makeAssoc vals weights = []

sample :: Random.RandomGen g => g -> SamplingDistribution a -> (g, a)
sample rgen dist = (rgen, dist !! 0) -- todo binary search on prob.

-- shouldI rgen prob
-- Returns whether a Bernoulli trial conducted by the parameter random
-- number generator was successful, with probability 'prob'. Returns
-- the updated generator as well.
shouldI :: Random.RandomGen g => g -> Double -> (g, Bool)

shouldI rand prob =
  let (x, rand') = Random.randomR (0, 1 :: Double) rand in (rand', x < prob)

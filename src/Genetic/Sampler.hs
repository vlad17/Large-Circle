-- Module that handles sampling from distributions

module Genetic.Sampler where

import Utils ((>|>))

import qualified Control.Monad as Monad
import qualified Control.Monad.Random as Random
import qualified Data.Function as Function
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Utils as Utils

-- TODO filter interface.

-- CDF upper bound to value mapping
type SamplingDistribution a = Map.Map Double a

-- makeAssoc vals weights
-- Makes a sampling distribution where the values 'vals' take on the
-- weights 'weights'. The normalized weights become the probability of
-- selection for the parameter values, after normalization. Weights
-- should be nonnegative and there should be as many in the list as there
-- are values (nonininfinite).
makeAssoc :: [a] -> [Double] -> SamplingDistribution a
makeAssoc vals weights =
  -- Descending sort before calculating CDF to hit likelies first.
  List.zip weights vals
  >|> List.sortBy (flip compare `Function.on` fst)
  >|> List.scanl1 (\ (prev, _) (next, v) -> (prev + next, v))
  >|> List.reverse
  >|> \ ls -> List.map (Utils.liftTup2 (/ (fst . List.head) ls) id) ls
  >|> Map.fromAscList

-- sample dist
-- samples the distribution 'dist'
sample :: (Functor m, Monad.Monad m, Random.RandomGen g) =>
          SamplingDistribution a -> Random.RandT g m a
sample dist = fmap pick unitR
  where pick prob = snd $ maybe (Map.findMax dist) id $ Map.lookupGE prob dist

-- shouldI prob
-- Returns whether a Bernoulli trial conducted by the parameter random
-- number generator was successful, with probability 'prob'.
shouldI :: (Functor m, Monad.Monad m, Random.RandomGen g) =>
           Double -> Random.RandT g m Bool

shouldI prob = fmap (< prob) unitR

unitR :: (Functor m, Monad.Monad m, Random.RandomGen g) =>
         Random.RandT g m Double
unitR = Random.getRandomR (0, 1 :: Double)

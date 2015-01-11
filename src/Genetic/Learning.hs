-- Exposes an iterative genetic learning interface.

module Genetic.Learning where

-- TODO: filter out interface. No Learner(..) needed.

import Data.Array.Unboxed ((!))
import Data.Bits ((.&.), (.|.))

import qualified Data.Array.Unboxed as UA
import qualified Data.Bits as Bits
import qualified Data.List as List
import qualified Data.Tuple as Tuple
import qualified Data.Word as Word
import qualified Genetic.Sampler as Sampler
import qualified System.Random as Random
import qualified Utils

-- Currently, each chromosome has a uniform length.
type Chromosome = UA.UArray Int Word.Word8

-- A learner represents a stage in the learning process
data Learner g = Learner { gen :: Int -- generation number
                         , random :: g -- random generator
                         , eval :: Chromosome -> Double -- fitness function
                         , crossover :: Double
                         , mutate :: Double
                         , chrLength :: Int
                         , number :: Int
                         , chromosomes :: [Chromosome] }

-- create rand fit cross mut len num
-- Generates a learner with the random number generator 'rand', fitness function
-- 'fit', and 'num' chromosomes initialized randomly with length 'len'.
-- 'cross' and 'mut' specify the crossover and mutation (for each bit)
-- probabilities during chromosome updates.
--
-- Length 'len' of each chromosome is measured in bytes.
--
-- If 'num' is negative or odd, it is increased until it is nonnegative and
-- even.
create :: Random.RandomGen g => g
          -> (Chromosome -> Double)
          -> Double
          -> Double
          -> Int
          -> Int
          -> Learner g

-- learn learner
-- Updates the state of the genetic algorithm in the parameter learner by one
-- step.
learn :: Random.RandomGen g => Learner g -> Learner g

-- TODO get best chromosome

-- TODO find a way to use a monad for transferring new rgen instances
-- instead of having a ton of rgen variables...

create rand fit cross mut len num = let
  num' = if num < 0 then 0 else num + num `mod` 2
  makeChromosome rgen = Utils.tupRight2 (UA.listArray (1, len)) $
                        Utils.feed (Tuple.swap . Random.random) rgen len
  (rgen', chr) = Utils.feed makeChromosome rand num'
  in Learner 0 rgen' fit cross mut len num' chr

learn learner = Learner (succ $ gen learner) rgen fit (crossover learner)
                (mutate learner) chrLen (number learner) picked
  where
    -- Extract the previous learner's data and build up a distribution
    (fit, old, chrLen) = (eval learner, chromosomes learner, chrLength learner)
    roulette = Sampler.makeAssoc old $ List.map fit old

    -- Various genetic update helper functions
    -- TODO add more documentation below
    -- TODO remove global 'rgen' variable, re-use parameter rgen name.
    fillBits = List.foldl Bits.setBit 0
    crossOver (rgenc, cr1, cr2) = let
      (crossByte, rgen') = Random.randomR (1, chrLen) rgenc
      (crossBit, rgen'') = Random.randomR (0, 7) rgen'
      loMask = fillBits [0..crossBit]
      hiMask = negate loMask
      pullSegment arr = map $ Utils.makeTup id ((UA.!) arr)
      centerByte arr1 arr2 =
        (crossByte, arr1!crossByte .&. loMask .|. arr2!crossByte .&. hiMask)
      crossedBytes arr1 arr2 = UA.array (1, chrLen) $
        pullSegment arr1 [1..pred crossByte] ++
        pullSegment arr2 [succ crossByte..chrLen] ++
        [centerByte arr1 arr2]
      in (rgen'', crossedBytes cr1 cr2, crossedBytes cr1 cr2)
    doMutate (rgend, cr1, cr2) = let
      makeBitWord rgenw =
        Utils.tupRight2 (fillBits . List.findIndices id) $
        Utils.feed (flip Sampler.shouldI $ mutate learner) rgenw 8
      makeBitFlip rgenb = Utils.feed makeBitWord rgenb chrLen
      mutateWord a b ix = a!ix `Bits.xor` b!!pred ix
      mutated a b =
        UA.listArray (1, chrLen) . map (mutateWord a b) $ [1..chrLen]
      (rgen', [bf1, bf2]) = Utils.feed makeBitFlip rgend 2
      in (rgen', [mutated cr1 bf1, mutated cr2 bf2])

    -- Pick chromosomes at random, cross over and mutate until a new pool forms
    pick rgenp = Sampler.sample rgenp roulette
    make2New rgenm = let
      (rgen', [cr1, cr2]) = Utils.feed pick rgenm 2
      (rgen'', shouldCrossOver) = Sampler.shouldI rgen' $ crossover learner
      in doMutate . Utils.doIf shouldCrossOver crossOver $ (rgen'', cr1, cr2)
    (rgen, picked) = Utils.tupRight2 concat $
      Utils.feed make2New (random learner) $ number learner `div` 2

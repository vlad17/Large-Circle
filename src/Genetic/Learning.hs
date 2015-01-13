-- Exposes an iterative genetic learning interface.

module Genetic.Learning where

-- TODO: filter out interface. No Learner(..) needed.

import Data.Array.Unboxed ((!))
import Data.Bits ((.&.), (.|.))

import qualified Control.Monad as Monad
import qualified Control.Monad.Random as Random
import qualified Data.Array.Unboxed as Array
import qualified Data.Bits as Bits
import qualified Data.List as List
import qualified Data.Word as Word
import qualified Genetic.Sampler as Sampler
--import qualified System.Random as Random
import qualified Utils

-- Currently, each chromosome has a uniform length.
-- TODO make this accept a generic Ix
type Chromosome = Array.UArray Int Word.Word8

-- A learner represents a stage in the learning process
data Learner g = Learner { gen :: Int -- generation number
                         , rand :: g -- random generator
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

create rgen fit cross mut len num = let
  num' = if num < 0 then 0 else num + num `mod` 2
  makeChromosome = fmap (Array.listArray (1, len)) $
                   Monad.replicateM len Random.getRandom
  (chrs, rgen') = Random.runRand (Monad.replicateM num' makeChromosome) rgen
  in Learner 0 rgen' fit cross mut len num' chrs

learn learner =
  let
    newPool = Monad.replicateM (number learner `div` 2) pick2
    (chrs, rgen) = Random.runRand newPool $ rand learner
  in Learner (succ $ gen learner) rgen fit (crossover learner) (mutate learner)
     chrLen (number learner) $ concatMap (\ (a, b) -> [a, b]) chrs
  where
    -- Extract the previous learner's data and build up a distribution
    (fit, old, chrLen) = (eval learner, chromosomes learner, chrLength learner)
    roulette = Sampler.makeAssoc old $ List.map fit old

    fillBits = List.foldl Bits.setBit 0

    crossOver (cr1, cr2) = do
      -- Choose a random crossing point
      crossByte <- Random.getRandomR (1, chrLen)
      crossBit <- Random.getRandomR (0, 7)
      let
        -- Generate byte masks for the crossed byte
        loMask = fillBits [0..crossBit]
        hiMask = negate loMask
        -- Helpers to cross over, parameterized by direction of copy.
        pullSegment arr = map $ Utils.makeTup id ((!) arr)
        crossedBytes arr1 arr2 = Array.array (1, chrLen) $
          pullSegment arr1 [1..pred crossByte] ++
          pullSegment arr2 [succ crossByte..chrLen] ++
          [(crossByte, arr1!crossByte .&. loMask .|. arr2!crossByte .&. hiMask)]
      return $ (crossedBytes cr1 cr2, crossedBytes cr1 cr2)

    doMutate (cr1, cr2) =
      let
        -- Generate bit lists with 1s appearing with rate 'mutate learner'
        makeBitWord = fmap (fillBits . List.findIndices id) $
                      Monad.replicateM 8 (Sampler.shouldI $ mutate learner)
        makeBitFlip = Monad.replicateM chrLen makeBitWord

        -- Bit-flipping is just an xor op. Note 'b' is a list.
        mutateWord a b ix = a!ix `Bits.xor` b!!pred ix
        mutated a b =
          Array.listArray (1, chrLen) . map (mutateWord a b) $ [1..chrLen]
      in do
        [bf1, bf2] <- Monad.replicateM 2 makeBitFlip
        return $ (mutated cr1 bf1, mutated cr2 bf2)

    -- Pick chromosomes at random, cross over and mutate until a new pool forms
    pick2 = do
      [cr1, cr2] <- Monad.replicateM 2 $ Sampler.sample roulette
      shouldCrossOver <- Sampler.shouldI $ crossover learner
      Utils.doIfM shouldCrossOver crossOver (cr1, cr2) >>= doMutate

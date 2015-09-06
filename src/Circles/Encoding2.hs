-- Module provides encoding and decoding functions for genetic learning.
-- This encoding is much more efficient than Encoding1, by only representing
-- the centers of the circles, and then using the provided enviornment to find
-- the maximum feasible radius possible.

module Circles.Encoding2 where

import Utils (minBits, readAdjacentBytes, (>|>))

import qualified Data.List as List
import qualified Data.Word as Word
import qualified Circles.Circles as Circles

-- codeSize w h
-- Returns the size of the code for a frame of size 'w' x 'h'.
-- All codes are of the same size.
codeSize :: Int -> Int -> Int

-- encode circle
-- Encodes a circle as a byte list.
-- encode :: Circles.Circle -> [Word.Word8]
-- TODO: implement, encoding only necessary for testing

-- decoder w h
-- Generates a decoder specialized for the given range.
decoder :: [Circles.Circle] -> Int -> Int -> ([Word.Word8] -> Circles.Circle)

-- Implementation

codeSize w h = map minBits [w, h] >|> List.sum >|> (+ 7) >|> (`quot` 8)

-- Serialization format is:
-- |---x---||---y---|

decoder circles w h = decode
  where
    decode bytes =
      let [x, y, r] = readAdjacentBytes bytes $ map minBits [w, h]
      in Circles.Circle (x `rem` w) (y `rem` h) (rem r $ Circles.maxR w h)

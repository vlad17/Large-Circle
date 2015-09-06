-- Module provides encoding and decoding functions for genetic learning.
-- This encoding is much more efficient than Encoding1, by only representing
-- the centers of the circles, and then using the provided enviornment to find
-- the maximum feasible radius possible.

module Circles.Encoding2 where

import Utils (minBits, readAdjacentBytes, (>|>))

import qualified Data.List as List
import qualified Data.Word as Word
import qualified Circles.Circles as Circles
import qualified Circles.CirclesArena as CirclesArena

-- codeSize w h
-- Returns the size of the code for a frame of size 'w' x 'h'.
-- All codes are of the same size.
codeSize :: Int -> Int -> Int

-- encode circle
-- Encodes a circle as a byte list.
-- encode :: Circles.Circle -> [Word.Word8]
-- TODO: implement, encoding only necessary for testing

-- decoder arena w h
-- Generates a decoder specialized for the given range and circles arena.
decoder :: CirclesArena.CirclesArena -> Int -> Int -> ([Word.Word8] -> Circles.Circle)

-- Implementation

codeSize w h = map minBits [w, h] >|> List.sum >|> (+ 7) >|> (`quot` 8)

-- Serialization format is:
-- |---x---||---y---|

decoder arena w h = decode
  where
    decode bytes =
      let [x, y] = readAdjacentBytes bytes $ map minBits [w, h]
          [x', y'] = [x `rem` w, y `rem` h]
          r = CirclesArena.maxPossibleRadius arena x y
          rAdjust = min x' . min (w - x') . min y' . min (h - y')          
      in Circles.Circle x' y' $ rAdjust r

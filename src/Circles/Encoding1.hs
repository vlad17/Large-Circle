-- Module provides encoding and decoding functions for genetic learning.

module Circles.Encoding1 where

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
decoder :: Int -> Int -> ([Word.Word8] -> Circles.Circle)

-- Implementation

codeSize w h =  map minBits [w, h, Circles.maxR w h] 
  >|> List.sum >|> (+ 7) >|> (`quot` 8)

-- Serialization format is:
-- |---x---||---y---||---r---|

decoder w h = decode
  where
    rFull = Circles.maxR w h
    decode bytes =
      let [x, y, r] = readAdjacentBytes bytes $ map minBits [w, h, rFull]
      in Circles.Circle (x `rem` w) (y `rem` h) (rem r $ rFull)

-- Module provides encoding and decoding functions for genetic learning.

module Circles.Encoding where

import Data.Bits ((.&.), (.|.))
import Utils ((>|>))

import qualified Data.Bits as Bits
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

codeSize w h = codeLengths w h >|> List.sum >|> (+ 7) >|> (`quot` 8)

-- Serialization format is:
-- |---x---||---y---||---r---|

decoder w h = decode
  where
    decode bytes =
      let [x, y, r] = pop bytes (codeLengths w h) 0
      in Circles.Circle (x `rem` w) (y `rem` h) (rem r $ Circles.maxR w h)
    pop _ [] _ = []
    pop [] _ _ = error "pop [] _ _: expected more bytes"
    pop (_:bs) ls 8 = pop bs ls 0
    pop bbs@(b:bs) (l:ls) offset =
      if l <= 8 - offset then
        byteExtract offset (offset + l) b : pop bbs ls (offset + l)
      else
        let
          afterFirst = l - 8 + offset
          follow = afterFirst `quot` 8
          (midBytes, nextBytes) = List.splitAt follow bs
          lastOffset = afterFirst `rem` 8
          firstByte = byteExtract offset 8 b
          lastByte = byteExtract 0 lastOffset $ head nextBytes
          iBytes = List.reverse midBytes ++ [firstByte]
        in toInt iBytes lastByte lastOffset : pop nextBytes ls lastOffset
    byteExtract lo hi b =
      let (bottom, top) = (8 - hi, 7 - lo)
      in List.foldl Bits.setBit 0 [bottom..top] .&. b
         >|> flip quot (2 ^ bottom) >|> fromIntegral
    toInt [] next offset = fromIntegral next `Bits.shiftL` offset
    toInt (u:us) next offset =
      toInt [] next offset .|. toInt us u (offset + 8)

-- Returns a triplet for the [x, y, r] bits needed to represent
codeLengths w h =
  let log2 0 = 0
      log2 x = 1 + log2 (x `quot` 2)
  in [log2 $ pred w, log2 $ pred h, log2 $ Circles.maxR w h]

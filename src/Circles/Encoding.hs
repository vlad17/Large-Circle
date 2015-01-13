-- Module provides encoding and decoding functions for genetic learning.

module Circles.Encoding where

import qualified Data.Binary as Binary

import qualified Data.Bits as Bits
import qualified Data.ByteString as B
import qualified Data.Word as Word
import qualified Circles.Circles

-- encode circle
-- Encodes a circle as a byte list.
encode :: Circles.Circle -> [Word.Word8]

-- decode code
-- Decodes a circle from the parameter byte list.
decode :: [Word.Word8] -> Circles.Circle

-- Implementation

encode circle = B.unpack . Binary.encode $ Circles.toTuple circle

decode code = let (x, y, r) = Binary.decode . B.pack $ code
              in Circles.Circle x y r

-- Module provides encoding and decoding functions for genetic learning.

module Circles.Encoding where

import qualified Data.Binary as Binary
import qualified Data.ByteString.Lazy as B
import qualified Data.List as List
import qualified Data.Word as Word
import qualified Circles.Circles as Circles

-- codeSize
-- Returns the encoding length, in bytes, for a circle.
codeSize :: Int

-- encode circle
-- Encodes a circle as a byte list.
encode :: Circles.Circle -> [Word.Word8]

-- decode code
-- Decodes a circle from the parameter byte list.
decode :: [Word.Word8] -> Circles.Circle

-- Implementation

-- TODO the encoding shouldn't just be the raw data. Only legal, in-range
-- values should exist (or ones nearly so).

codeSize = List.length . encode $ Circles.Circle 0 0 0

encode circle = B.unpack . Binary.encode $ Circles.toTuple circle

decode code = let (x, y, r) = Binary.decode . B.pack $ code
              in Circles.Circle x y r

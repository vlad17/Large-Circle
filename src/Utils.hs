-- Common utility functions I've needed.

module Utils where

import Data.Bits ((.&.), (.|.))

import qualified Data.Bits as Bits
import qualified Data.List as List
import qualified Data.Word as Word

liftTup2 :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
liftTup2 f g (x, y) = (f x, g y)

makeTup :: (a -> b) -> (a -> c) -> a -> (b, c)
makeTup f g x = (f x, g x)

mapTup :: (a -> b) -> (a, a) -> (b, b)
mapTup f = liftTup2 f f

doIf :: Bool -> (a -> a) -> a -> a
doIf b f x = if b then f x else x

doIfM :: Monad m => Bool -> (a -> m a) -> a -> m a
doIfM b f x = if b then f x else return x

infixl 1 >|>
(>|>) :: a -> (a -> b) -> b
(>|>) x f = f x

square :: Num a => a -> a
square x = x * x

isqrt :: Int -> Int
isqrt = floor . (sqrt :: Double -> Double) . fromIntegral

log2 :: Int -> Int
log2 0 = 0
log2 x = 1 + log2 (x `quot` 2)

minBits :: Int -> Int
minBits = log2 . pred

readAdjacentBytes :: [Word.Word8] -> [Int] -> [Int]
readAdjacentBytes bytes fixedWidths = pop bytes fixedWidths 0 where
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

-- Problems 31 to 41
-- https://www.haskell.org/haskellwiki/99_questions/31_to_41

module TutorialProblems.Problems31to41 where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Tuple as Tuple

primes :: Integral a => [a]
primes = next [2..]
  where next (n:ns) = n : (next . filter ((/= 0) . flip mod n) $ ns)

liftTup2 :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
liftTup2 f g (x, y) = (f x, g y)

makeTup :: (a -> b) -> (a -> c) -> a -> (b, c)
makeTup f g x = (f x, g x)

-- Problem 31
isPrime :: Integral a => a -> Bool
isPrime x
  | x < 2 = False
  | otherwise = List.foldl prime_check True [2..isqrt x]
    where prime_check b n = b && x `mod` n /= 0
          isqrt = floor . sqrt . fromIntegral

-- Problem 32
myGCD :: Integral a => a -> a -> a
myGCD a b = binGCD (abs a) (abs b)
    where
      binGCD x 0 = x
      binGCD 0 y = y
      binGCD x y
        | x == y = x
        | even x && even y = 2 * binGCD (x `div` 2) (y `div` 2)
        | even x = binGCD (x `div` 2) y
        | even y = binGCD x (y `div` 2)
        | otherwise = binGCD (max x y - min x y) (min x y)

-- Problem 33
coprime :: Integral a => a -> a -> Bool
coprime = (== 1) .: gcd where (.:) = ((.) . (.)) -- double composition

-- Problem 34
totient :: Int -> Int
totient 1 = 1
totient n = length $ filter (coprime n) [1..pred n]

-- Problem 35
primeFactors :: Integral a => a -> [a]
primeFactors = aux 2
  where aux n x
          | abs x < 2 = []
          | x `mod` n == 0 = n : aux n (x `div` n)
          | otherwise = aux (succ n) x

-- Problem 36
primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult = map (makeTup head length) . List.group . primeFactors

-- Problem 37
totient2 :: Int -> Int
totient2 = foldl mult_tot 1 . primeFactorsMult
  where mult_tot t (p, m) = t * pred p * p ^ pred m

-- Problem 38
compareTotients :: Bool
compareTotients = totient 10090 == totient2 10090

-- Problem 39
primesR :: Integral a => a -> a -> [a]
primesR x y = dropWhile (< x) . takeWhile (< y) $ primes

-- Problem 40
goldbach :: Integral a => a -> (a, a)
goldbach x
  | even x && x /= 2 = head [ (p, x - p) | p <- primes, isPrime $ x - p ]
  | otherwise = (x, 0)

-- Problem 41
goldbachList :: Integral a => a -> a -> [(a, a)]
goldbachList lo hi = let elo = lo + lo `mod` 2
                     in map goldbach [elo, elo + 2 .. hi]

goldbachListFast :: Integral a => a -> a -> [(a, a)]
goldbachListFast lo hi =
  let fwd_primes = primesR 0 hi
      rev_primes = reverse fwd_primes
      in_range [] _ s = s
      in_range _ [] s = s
      in_range fwd@(f:fs) rev@(r:rs) s
        | f > r = s
        | f + r > hi = in_range fwd rs s
        | otherwise = in_range fs rev $ add_all f rev s
      add_all f rs s = foldl add_one s rs
        where add_one s r =
                if f + r < lo || odd (f + r) || Map.member (f + r) s then s
                else Map.insert (f + r) (f, r) s
  in Map.elems $ in_range fwd_primes rev_primes Map.empty

goldbachList' :: Integral a => a -> a -> a -> [(a, a)]
goldbachList' lo hi min = filter bigger $ goldbachListFast lo hi
  where bigger (x, y) = x > min && y > min

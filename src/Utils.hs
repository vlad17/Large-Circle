-- Common utility functions I've needed.

module Utils where

liftTup2 :: (a -> b) -> (c -> d) -> (a, c) -> (b, d)
liftTup2 f g (x, y) = (f x, g y)

tupLeft2 f = liftTup2 f id
tupRight2 g = liftTup2 id g

makeTup :: (a -> b) -> (a -> c) -> a -> (b, c)
makeTup f g x = (f x, g x)

mapTup :: (a -> b) -> (a, a) -> (b, b)
mapTup f = liftTup2 f f

revTup :: (a, b) -> (b, a)
revTup (a, b) = (b, a)

feed :: (a -> (a, b)) -> a -> Int -> (a, [b])
feed f input 0 = (input, [])
feed f input n
  | n < 0 = error "feed f input n: n < 0"
  | otherwise = (final, out:prev)
    where (next, out) = f input
          (final, prev) = feed f next $ pred n

doIf b f x = if b then f x else x

--worldJoin :: (a -> (a, b)) -> (a -> (c, d)) -> (a -> (c, (b, d)))
--worldJoin f g = \ x -> joined x
--  where joined x = (final, (outf, outg))
--          where (next, outf) = f x
--                (final, outg) = g next

nest :: (a -> a) -> a -> Int -> a
nest f x 0 = x
nest f x n
  | n < 0 = error "nest f x n: n < 0"
  | otherwise = f $ nest f x $ pred n

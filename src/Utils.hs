-- Common utility functions I've needed.

module Utils where

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

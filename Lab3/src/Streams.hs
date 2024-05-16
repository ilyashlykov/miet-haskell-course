{-# OPTIONS_GHC -Wall #-}
module Streams where

import Data.List(intercalate)

-- Задание 1 -----------------------------------------

data Stream a = a :> Stream a

instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ sTake 10 s) ++ ", ..."

streamToList :: Stream a -> [a]
streamToList (x :> xs) = x : streamToList xs

sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake n (x :> xs) = x : sTake (n - 1) xs

-- Задание 2 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = x :> sRepeat x

sCycle :: [a] -> Stream a
sCycle xs = cycleStream xs where
  cycleStream [] = cycleStream xs
  cycleStream (y:ys) = y :> cycleStream ys

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = x :> sIterate f (f x)

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (x :> xs) ys = x :> sInterleave ys xs

-- Задание 3 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = rulerHelper 0
  where
    rulerHelper n = sInterleave (sRepeat n) (rulerHelper (n + 1))

-- Задание 4 -----------------------------------------

minMaxSlow, minMax, minMaxBang :: Ord a => [a] -> Maybe (a, a)

minMaxSlow [] = Nothing
minMaxSlow xs = Just (minimum xs, maximum xs)

minMax [] = Nothing
minMax (x:xs) = Just $ foldl (\(mn, mx) y -> (min mn y, max mx y)) (x, x) xs

minMaxBang [] = Nothing
minMaxBang (x:xs) = Just $ foldl (\(mn, mx) y -> (mn `seq` min mn y, mx `seq` max mx y)) (x, x) xs

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 ruler

-- Задание 6 -----------------------------------------

instance Functor Stream where
    fmap f (x :> xs) = f x :> fmap f xs

instance Applicative Stream where
    pure = sRepeat
    (f :> fs) <*> (x :> xs) = f x :> (fs <*> xs)

instance Monad Stream where
    return = pure
    xs >>= f = join (fmap f xs)
      where
        join ((x :> xs) :> xss) = x :> sInterleave xs (join xss)

instance Foldable Stream where
    foldr f z (x :> xs) = f x (foldr f z xs)

instance Traversable Stream where
    traverse f (x :> xs) = (:>) <$> f x <*> traverse f xs

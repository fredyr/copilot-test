{-# LANGUAGE RebindableSyntax #-}
module Utils where
import Language.Copilot


max :: (Typed a, Ord a) => Stream a -> Stream a -> Stream a
max x y = mux ( x >= y ) x y

min :: (Typed a, Ord a) => Stream a -> Stream a -> Stream a
min x y = mux ( x < y ) x y

clamp low high val = Utils.min high $ Utils.max low val

warn :: (Typed a, Ord a) => Stream a -> Stream a -> Stream Bool
warn limit temp = temp > limit

decay :: Stream Word8 -> Stream Bool -> Stream Bool
decay n b = y
  where
  cnt  = [0] ++ cnt' :: Stream Word8
  cnt' = if b then n
         else if (cnt > 0) then cnt - 1
              else 0
  y = mux (cnt > 0 || b) true false

-- Hysteresis
hysteresis :: (Typed a, Ord a) => Stream a -> Stream a -> Stream a -> Stream Bool
hysteresis low high temp = y
  where
    hs = [True] ++ y
    y = if temp > high && (not hs) then true
        else if temp < low && hs then false
             else hs

-- Detect changes from incoming stream and convert that to signal
impulse :: Stream Bool -> Stream Bool
impulse x = y
  where
    x' = [True] ++ x
    y = (x' /= x)

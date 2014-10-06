{-# LANGUAGE RebindableSyntax #-}

module FIRCopilot ( fircopilot ) where

import Language.Copilot
import qualified Prelude as P

spec :: Spec
spec = do
	observer "0_debug_nats" near4
	observer "2_debug_min" $ fir coffs ([0,0,0] ++ near4)
	observer "3_debug_min" $ firSimple ([0,0,0] ++ near4)
	 
nats :: Stream Float
nats = [0] ++ (nats + 1)

near4 :: Stream Float
near4 = [4,4,4.2,4,4,3.8] ++ near4

natsDelayed :: Stream Float
natsDelayed = [0,0,0,0,0] ++ nats

fir :: (Eq a, Num a, Typed a) => [Stream a] -> Stream a -> Stream a
fir coffs s =  P.foldl1 (+) $ P.zipWith (*) coffs $ take n s
	where n = length coffs

coffs :: [Stream Float]
coffs = [b0, b1, b2]
	where
		b0 = 0.33
		b1 = 0.33
		b2 = 0.33

bajs n s = P.foldl (+) 0 0
	where
		zw = (P.zipWith (*) (tails s) (tails s))


firSimple :: Stream Float -> Stream Float
firSimple s = ((drop 0 s) * b0) + ((drop 1 s) * b1) + ((drop 2 s) * b2) 
	where
		b0 :: Stream Float
		b0 = 0.33
		b1 = 0.33
		b2 = 0.33

fircopilot :: IO ()
fircopilot = do
	putStrLn "Hello World"
	prettyPrint spec
	putStrLn ""
	interpret 20 spec

main :: IO()
main = fircopilot 

run = fircopilot
{-# LANGUAGE RebindableSyntax #-}

module TempMux ( tempMux ) where

import Language.Copilot
import qualified Prelude as P

chATemp :: Stream Int32
chATemp = extern "e_chATemp" (Just [-15,-10..])

chBTemp :: Stream Int32
chBTemp = extern "e_chBTemp" (Just [50,47..])

maxTemp :: (Typed a, Ord a) => Stream a -> Stream a -> Stream a 
maxTemp x y = mux ( x >= y ) x y

tempWarning :: (Typed a, Ord a) => Stream a -> Stream a -> Stream Bool
tempWarning limit temp = temp > limit

warningDecay ::  Stream Bool -> Stream Bool
warningDecay b = y
  where
  cnt  = [0] ++ cnt' :: Stream Word8
  cnt' = if b then 5 
  			else if (cnt > 0) then cnt - 1
  				              else 0
  y = mux (cnt > 0 || b) true false

counter :: (Eq a, Num a, Typed a) => Stream Bool -> Stream a
counter reset = y
  where
  zy = [0] ++ y
  y  = if reset then 0 else zy + 1


spec :: Spec
spec = do
	 
	observer "0_debug_chATemp" chATemp 
	observer "1_debug_chBTemp" chBTemp 
	observer "2_debug_maxTempAll" maxTempAll
	observer "3_debug_warning" warning
	observer "4_debug_warning holded" warningHolded

	where
		tempLimit = 46
		maxTempAll = maxTemp chATemp chBTemp
		warning = tempWarning tempLimit maxTempAll
		warningHolded = warningDecay warning



tempMux :: IO ()
tempMux = do
	putStrLn "Hello World"
	prettyPrint spec
	putStrLn ""
	interpret 20 spec

main :: IO()
main = tempMux 
{-# LANGUAGE RebindableSyntax #-}

import Language.Copilot
import qualified Prelude as P
  
-- 64 taps
taps = [0.002027487043, -0.0003529389707, -0.00212972605, -0.0007189568184, 0.002339297908, 0.001307839601, -0.003179958563, -0.003042120002,
        0.003061612412, 0.00468097648, -0.002856371702, -0.007128477879, 0.001418827809, 0.009415056872, 0.0007981110217, -0.01189613048,
        -0.004603383873, 0.01367141084, 0.009758658271, -0.01464650859, -0.01686326359, 0.01382000782, 0.02595293462, -0.01050082402,
        -0.03801842451, 0.002735098608, 0.05492639067, 0.01356583114, -0.08435662306, -0.05597996792, 0.1756444889, 0.4197493789,
        0.4197493789, 0.1756444889, -0.05597996792, -0.08435662306, 0.01356583114, 0.05492639067, 0.002735098608, -0.03801842451,
        -0.01050082402, 0.02595293462, 0.01382000782, -0.01686326359, -0.01464650859, 0.009758658271, 0.01367141084, -0.004603383873,
        -0.01189613048, 0.0007981110217, 0.009415056872, 0.001418827809, -0.007128477879, -0.002856371702, 0.00468097648, 0.003061612412,
        -0.003042120002, -0.003179958563, 0.001307839601, 0.002339297908, -0.0007189568184, -0.00212972605, -0.0003529389707, 0.002027487043]

blip :: Stream Double
blip = replicate 64 0 ++ b
  where b = [1,1,1,1] ++ b

zeros :: Stream Double
zeros = replicate 64 0 ++ zeros

alts :: Stream Bool
alts = [True,False,True,False,True,False,True,False,True,False,True,False,True,False,True,False,True,False,True,False,True,False,True,False,True,False,True,False,True,False,True,False,True,False,True,False,True,False,True,False,True,False,True,False,True,False] ++ alts

fir :: Stream Double -> Stream Double
fir xs = foldl1 (+) ps
  where ps = zipWith (*) taps $ take (length taps) xs

firKalle :: (Eq a, Num a, Typed a) => [Stream a] -> Stream a -> Stream a
firKalle coffs s =  P.foldl1 (+) $ P.zipWith (*) coffs $ take n s
	where n = length coffs

upsample :: Stream Bool -> Stream Double -> Stream Double
upsample bs ds = zs
  where ms = mux bs ds $ zeros
        zs = replicate 64 0 ++ ms

test :: Spec
test = do
  observer "p" $ blip
  observer "f" $ fir blip
  observer "f'" $ firKalle taps blip
  observer "a" $ alts
  observer "4" $ fir $ upsample alts blip

runTest :: IO ()
runTest = interpret 100 test

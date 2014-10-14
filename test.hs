{-# LANGUAGE RebindableSyntax #-}
import Language.Copilot
import qualified Prelude as P

cplexMulS :: (Stream Word64, Stream Word64)
        -> (Stream Word64, Stream Word64)
        -> (Stream Word64, Stream Word64)
cplexMulS (a,b) (c,d) = (a*c - b*d, a*d + b*c)

fstS :: (Stream a, Stream b) -> Stream a
fstS (a, b) = a

sndS :: (Stream a, Stream b) -> Stream b
sndS (a, b) = b

nats :: Stream Word64
nats = [0] ++ (nats + 1)

blip :: Stream Word64
blip = [3,3,3,7] ++ blip

warn :: Stream Word64 -> Word64 -> Stream Bool
warn vals limit = do
  if (vals > constant limit) then true else false

decay :: (Stream Word64, Stream Bool) -> (Stream Word64, Stream Bool)
decay (ds, vs) = (ds', vs')
  where
    t = [True] ++ vs
    n = [3] ++ ds
    z = [0] ++ ds
    ds' = if ds == 0 then 
            if vs then n else z
          else ds - 1
    vs' = if ds == 0 then
            if vs then t else not t 
          else t
                              
cnt :: Stream Word64
cnt = [0,0,0,0,2,1,0,0] ++ cnt

test :: Spec
test = do
    -- observer "w" $ sndS $ decay (constant 0, (warn blip 3))
    -- observer "v" $ fstS $ decay (constant 0, (warn blip 3))
    -- observer "b" $ warn blip 3 
  -- observer "c" $ (nuts :: Stream Word64)
  observer "p" $ prev nats
  observer "fib" $ (fib :: Stream Word64)
  observer "cnt" $ counter true false
  
runTest :: IO ()
runTest = interpret 20 test

-- hayoo.fh

-- Initialize stream
(~>) :: (Typed a) => a -> Stream a -> Stream a
x ~> xs = [x] ++ xs
-- Previous stream value
prev :: (Typed a, Num a) => Stream a -> Stream a
prev a = [0] ++ a

-- nuts :: (Typed a, Eq a, Num a) => Stream a
-- nuts = n
--   where n = [0] ++ nuts + 1

fib :: (Typed a, Eq a, Num a) => Stream a
fib = bdy
  where
    bdy = 1 ~> rst
    rst = prev bdy + bdy

counter :: Stream Bool -> Stream Bool -> Stream Word64
counter inc reset = cnt
  where
    cnt = if reset then 0
          else if inc then z + 1
               else z
    z = 0 ~> cnt

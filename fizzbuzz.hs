{-# LANGUAGE RebindableSyntax #-}

import Language.Copilot
import qualified Prelude as P


nats = [1] ++ nats + constW32 1
fizz a = a `mod` 3 == 0
buzz a = a `mod` 5 == 0
fizzbuzz a = fizz a && buzz a
             
fizzbuzzer :: Stream Word32 -> Stream Word8
fizzbuzzer a = z
  where z = if fizzbuzz a
            then fb
            else if fizz a
                 then f
                 else if buzz a
                      then b
                      else n
        n  = 0 -- numeric
        f  = 1 -- "fizz"
        b  = 2 -- "buzz"
        fb = 3 -- "fizzbuzz"

fibu :: Spec
fibu = do
  observer "n" $ nats
  observer "fibu" $ fizzbuzzer nats
  trigger "fizzbuzz" true [arg $ fizzbuzzer nats, arg nats]
  

runFB :: IO ()
runFB = interpret 20 fibu

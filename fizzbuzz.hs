{-# LANGUAGE RebindableSyntax #-}

import Language.Copilot
import qualified Prelude as P

nats :: Stream Int32
nats = [1] ++ nats + 1

{- You there not allowed to do this, expected type 'Bool', not type 'Stream Bool'
fizzbuzz2 n | mod n 15 == 0 = 99
			      | mod n 5  == 0 = 88
            | mod n 3  == 0 = 77
            | otherwise = n
-}

fizzbuzz n =  if   		  mod n 15 == 0 then -3
              else if 	mod n 5  == 0 then -2
			        else if 	mod n 3  == 0 then -1
			        else n

-- -1 "Fizz"
-- -2 "Buzz"
-- -3 "FizzBuzz"


spec :: Spec
spec = do
	observer "0_nats" nats
	observer "1_fizzbuzz" $ fizzbuzz nats



run :: IO ()
run = do
  putStrLn "PrettyPrinter:"
  putStrLn ""
  prettyPrint spec
  putStrLn ""
  putStrLn ""
  putStrLn "Interpreter:"
  putStrLn ""
  interpret 20 spec

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

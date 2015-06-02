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


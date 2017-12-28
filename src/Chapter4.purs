module Chapter4 where

import Prelude

import Data.Array (null)
import Data.Array.Partial (tail)
import Data.List (List, range)
import Partial.Unsafe (unsafePartial)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = (fib $ n - 1) + (fib $ n - 2)

firstTenFib :: List Int
firstTenFib = map fib $ range 1 10

length :: forall a. Array a -> Int
length arr = 
    if null arr
    then 0 
    else 1 + length (unsafePartial tail arr)

length2 :: forall a. Array a -> Int
length2 [] = 0
length2 xs = 1 + length (unsafePartial tail xs)
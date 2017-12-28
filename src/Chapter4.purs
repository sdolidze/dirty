module Chapter4 where

import Prelude

import Data.Array (null)
import Data.Array.Partial (head, tail)
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

even :: Int -> Boolean
even 0 = true
even 1 = false
even x = not $ even (x - 1)

evens :: Array Int -> Array Int
evens [] = []
evens arr =
    if even x
    then [x] <> evens xs
    else evens xs
    where
      x = (unsafePartial head arr)
      xs = (unsafePartial tail arr)

evensCount :: Array Int -> Int
evensCount [] = 0
evensCount arr =
    if even x
    then 1 + evensCount xs
    else evensCount xs
    where
      x = (unsafePartial head arr)
      xs = (unsafePartial tail arr)

plusOne :: Array Int -> Array Int
plusOne = map (\x -> x + 1)

-- `<$>` is inffix version of `map`
plusOne2 :: Array Int
plusOne2 = (\n -> n + 1) <$> [1, 2, 3, 4, 5]

infixr 8 range as ..
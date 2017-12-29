module Chapter4 where

import Prelude

import Control.MonadZero (guard)
import Data.Array (concat, concatMap, null, range, cons)
import Data.Array.Partial (head, tail)
import Data.Foldable (foldl, foldr)
import Data.List as L
import Data.Tuple (Tuple(..))
import Math (abs)
import Partial.Unsafe (unsafePartial)

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = (fib $ n - 1) + (fib $ n - 2)

firstTenFib :: L.List Int
firstTenFib = map fib $ L.range 1 10

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

--

concated :: Array Int
concated = concat [[1, 2, 3], [4], [5]]

concatMapped :: Array Int
concatMapped = concatMap (\x -> [x, x * x, 0]) [1, 2, 3]

-- 4.9

pairs :: Int -> Array (Array Int)
pairs n = do
  i <- 1 .. n
  j <- 1 .. n
  pure [i, j]

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- 1 .. n
  guard $ i * j == n
  pure [i, j]

factors2 :: Int -> Array Int
factors2 n = do
    i <- 1 .. n
    guard $ n `mod` i == 0
    pure i

isPrime :: Int -> Boolean
isPrime x = 
    if x <= 1
    then false
    else length (factors2 x) == 2

cartesianProduct :: forall a b. Array a -> Array b -> Array (Tuple a b)
cartesianProduct xs ys = do
    x <- xs
    y <- ys
    pure $ Tuple x y

pythagoreanTriples :: Int -> Array (Array Int)
pythagoreanTriples n = do
  a <- 1 .. n
  b <- 1 .. n
  c <- 1 .. n
  guard $ a * a + b * b == c * c
  pure [a, b, c]

-- 4.12

sum :: Array Int -> Int
sum = foldl (+) 0

reverseConcat :: Array Int -> String
reverseConcat = foldr (\n acc -> acc <> show n) ""

-- 4.13

fact2 :: Int -> Int 
fact2 n = fact' n 1
    where
        fact' 0 acc = acc
        fact' n acc = fact' (n - 1) (n * acc)

reverse :: forall a. Array a -> Array a
reverse = reverse' []
    where
      reverse' acc [] = acc
      reverse' acc xs = reverse' (cons (unsafePartial head xs) acc)
                                 (unsafePartial tail xs)
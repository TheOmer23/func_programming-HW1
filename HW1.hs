-- Implement the following functions.
-- When you're done, ghc -Wall -Werror HW0.hs should successfully compile.
--
-- Tells HLS to show warnings, and the file won't be compiled if there are any warnings, e.g.,
-- eval (-- >>>) won't work.
{-# OPTIONS_GHC -Wall -Werror #-}
-- Refines the above, allowing for unused imports.
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# LANGUAGE TemplateHaskell #-}

module HW1 where

-- These import statement ensures you aren't using any "advanced" functions and types, e.g., lists.
import Prelude (Bool (..), Eq (..), Int, Integer, Num (..), Ord (..), div, error, even, flip, id, mod, not, otherwise, undefined, ($), (&&), (.), (||))
import Data.ByteString (count, null)

------------------------------------------------
-- DO NOT MODIFY ANYTHING ABOVE THIS LINE !!! --
------------------------------------------------

-- ********* --
-- Section 1
-- ********* --

-- <stderr>: hPutChar: invalid argument (cannot encode character '\8226')
const :: a -> b -> a
const x _ = x
(.>) :: (a -> b) -> (b -> c) -> a -> c
f .> g = \x -> g (f x)
curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)
uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c
rotate :: (a -> b -> c -> d) -> c -> a -> b -> d
rotate f c a b = f a b c
lotate :: (a -> b -> c -> d) -> b -> c -> a -> d
lotate f b c a = f a b c

-- Generalizations of (.)
(.:) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.:) f g a b c =  f (g a b c)
(.:.) :: (e -> f) -> (a -> b -> c -> d -> e) -> a -> b -> c -> d -> f
(.:.) g h a b c d = g (h a b c d)
(.::) :: (f -> g) -> (a -> b -> c -> d -> e -> f) -> a -> b -> c -> d -> e -> g
(.::) h i a b c d e = h (i a b c d e) 
(.::.) :: (g -> h) -> (a -> b -> c -> d -> e -> f -> g) -> a -> b -> c -> d -> e -> f -> h
(.::.) i j a b c d e f = i (j a b c d e f) 
-- How can we ever implement such a function!?
impossible :: a -> b
impossible = undefined

-- ********* --
-- Section 2
-- *********--

-- >>> nullGen ((+1), (<0), 0)
-- <stderr>: hPutChar: invalid argument (cannot encode character '\8226')


-- >>> fromBinary (-1010)
-- -10

-- >>> isAbundant 15
-- False

-- >>> rotateDigits 0
-- 0

countDigits :: Integer -> Integer
countDigits a | a > 9 = 1 + countDigits(a `div` 10)
countDigits a | a < 0 = 1 + countDigits((-a) `div` 10) 
countDigits _ = 1

toBinary :: Integer -> Integer
toBinary 0 = 0
toBinary 1 = 1
toBinary a | a > 1 = toBinary(a `div` 2)*10 + (a `mod` 2)
toBinary a = (toBinary((-a)`div` 2)*10 + (a `mod` 2))*(-1)

fromBinary :: Integer -> Integer
fromBinary 0 = 0
fromBinary 1 = 1
fromBinary a | a > 1 = fromBinary(a `div` 10)*2 + (a `mod` 2)
fromBinary a = - fromBinary(-a)

isAbundant :: Integer -> Bool
isAbundant n
  | n <= 0    = False  -- Return False for non-positive integers
  | otherwise = sumOfDivisors (n - 1) 0 > n
  where
    sumOfDivisors :: Integer -> Integer -> Integer  
    sumOfDivisors 0 sum = sum
    sumOfDivisors a sum
      | n `mod` a == 0 = sumOfDivisors (a - 1) (sum + a)
      | otherwise      = sumOfDivisors (a - 1) sum

rotateDigits :: Integer -> Integer
rotateDigits n 
  | n < 0 = -(((-n) `div` 10) + ((- n) `mod` 10) * powerN 10 (countDigits n - 1))
  | otherwise = (n - (n `div` powerN 10 (countDigits n - 1)) * powerN 10 (countDigits n - 1)) * 10 + (n `div` powerN 10 (countDigits n - 1))

-- >>> powerN 2 8
-- 256
powerN :: Integer -> Integer -> Integer
powerN base exp
  | exp == 0 = 1
  | exp == 1 = base
  | otherwise = powerN base (exp-1) * base

-- ********* --
-- Section 3
-- ********* --

type Generator a = (a -> a, a -> Bool, a)
nullGen :: Generator a -> Bool
nullGen (_, p, a) = not (p a)
lastGen :: Generator a -> a
lastGen (f, p, a) = if nullGen (f, p, a) then a else lastGen (f, p, f a)
lengthGen :: Generator a -> Int
lengthGen (f, p, a) = if nullGen (f, p, a) then 0 else 1 + lengthGen (f, p, f a)
sumGen :: Generator Integer -> Integer
sumGen (f, p, a) = if nullGen (f, p, a) then 0 else f a + sumGen (f, p, f a) 

type Predicate a = a -> Bool
anyGen :: Predicate a -> Generator a -> Bool
anyGen q (f,p,a)
  | nullGen (f,p,a) = False
  | q (f a)         = True
  | otherwise       = anyGen q (f,p,f a)
allGen :: Predicate a -> Generator a -> Bool
allGen q (f,p,a)
  | nullGen (f,p,a) = True
  | not (q (f a))   = False
  | otherwise       = allGen q (f,p,f a)
noneGen :: Predicate a -> Generator a -> Bool
noneGen q (f,p,a) = not (anyGen q (f,p,a)) 

countGen :: Predicate a -> Generator a -> Int
countGen q (f,p,a) 
  | nullGen (f,p,a) = 0
  | q (f a)         = 1 + countGen q (f,p,f a)
  | otherwise       = countGen q (f,p,f a)
-- ********* --
-- Section 4
-- ********* --
isPrime :: Integer -> Bool
isPrime = undefined
isSemiprime :: Integer -> Bool
isSemiprime = undefined
goldbachPair :: Integer -> (Integer, Integer)
goldbachPair = undefined
goldbachPair' :: Integer -> (Integer, Integer)
goldbachPair' = undefined

-- ***** --
-- Bonus
-- ***** --
isCircularPrime :: Integer -> Bool
-- If you choose the implement this function, replace this with the actual implementation
isCircularPrime = undefined

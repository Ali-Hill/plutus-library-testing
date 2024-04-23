{-@ LIQUID "--reflection"     @-}
{-@ LIQUID "--ple"            @-}
{-@ LIQUID "--no-termination" @-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Main where

import PlutusCore.Builtin (Insert)
import Prelude hiding (Enum (..), Rational, negate, recip)

main :: IO ()
main = putStrLn "test"

{-@ type Pos = {v:Int | 0 < v} @-}

{-@ (==>) :: p:Bool -> q:Bool -> {v:Bool | v <=> (p ==> q)} @-}
False ==> False = True
False ==> True  = True
True  ==> True  = True
True  ==> False = False

{-@ (<=>) :: p:Bool -> q:Bool -> {v:Bool | v <=> (p <=> q)} @-}
False <=> False = True
False <=> True  = False
True  <=> True  = True
True  <=> False = False

{-@ incr :: Pos -> Pos @-}
incr :: Int -> Int
incr x = x + 1

{-@ type TRUE  = {v:Bool | v    } @-}
{-@ type FALSE = {v:Bool | not v} @-}

{-@ ex0 :: TRUE @-}
ex0 :: Bool
ex0 = True

{-@ ex1 :: Bool -> TRUE @-}
ex1 :: Bool -> Bool
ex1 b = b || not b

{-@ ex3 :: Bool -> Bool -> TRUE @-}
ex3 :: Bool -> Bool -> Bool
ex3 a b = if a then a else not (a && b)

-- can use == instead of <=> here
{-@ exDeMorgan1 :: Bool -> Bool -> TRUE @-}
exDeMorgan1 :: Bool -> Bool -> Bool
exDeMorgan1 a b = not (a || b) == (not a && not b)

{-@ ax6 :: Int -> Int -> TRUE @-}
ax6 :: Int -> Int -> Bool
ax6 x y = False ==> (x <= x + y)

{-@ ax1 :: Int -> TRUE @-}
ax1 :: Int -> Bool
ax1 x = x < x + 1

{-@ measure size @-}
{-@ size :: [a] -> Nat @-}
size        :: [a] -> Int
size []     = 0
size (_:xs) = 1 + size xs

{-@ fx0 :: [a] -> [a] -> TRUE @-}
fx0 :: Eq a => [a] -> [a] -> Bool
fx0 xs ys = (xs == ys) ==> (size xs == size ys)

{-@ fx2 :: a -> [a] -> TRUE @-}
fx2 :: a -> [a] -> Bool
fx2 x xs = 0 < size ys
  where
    ys   = x : xs


{-@ type Zero    = {v:Int | v == 0} @-}
{-@ type NonZero = {v:Int | v /= 0} @-}
{-@ zero :: Zero @-}
zero :: Int
zero = 0

-- This is what we need
{-@ type Nat   = {v:Int | 0 <= v} @-}
{-@ type Even  = {v:Int | v mod 2 == 0 } @-}
{-@ type Lt100 = {v:Int | v < 100}       @-}

{-@ die :: {v:String | false} -> a  @-}
die :: String -> a
die = error

-- This will return false if die is ever called
cannotDie :: ()
cannotDie = if 1 + 1 == 3
              then die "horrible death"
              else ()

{-@ divide :: Int -> NonZero -> Int @-}
divide     :: Int -> Int -> Int
divide n 0 = die "divide by zero"
divide n d = n `div` d

avg       :: [Int] -> Int
avg xs    = if n == 0 then 0 else divide total n
  where
    total = sum xs
    n     = length xs

-- postcondition example
{-@ abs' :: Int -> Nat @-}
abs'           :: Int -> Int
abs' n
  | 0 < n     = n
  | otherwise = 0 - n


{-@ isPositive :: x:Int -> {v:Bool | v <=> x > 0} @-}
isPositive :: Int -> Bool
isPositive x = x > 0

result :: Int -> Int -> [Char]
result n d
  | isPositive d = "Result = " ++ show (n `divide` d)
  | otherwise    = "Humph, please enter positive denominator!"

{-@ lAssert  :: {x:Bool | x == True} -> a -> a @-}
lAssert :: Bool -> p -> p
lAssert True  x = x
lAssert False _ = die "yikes, assertion fails!"

yes :: ()
yes = lAssert (1 + 1 == 2) ()
-- no  = lAssert (1 + 1 == 3) ()

truncate :: Int -> Int -> Int
truncate i max
  | i' <= max' = i
  | otherwise  = max' * (i `divide` i')
    where
      i'       = abs' i
      max'     = abs' max

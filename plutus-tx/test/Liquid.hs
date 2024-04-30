{-@ LIQUID "--reflection"     @-}
{-@ LIQUID "--ple"            @-}
{-@ LIQUID "--no-termination" @-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ImplicitParams      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Main where

import PlutusCore.Builtin (Insert)
import Prelude hiding (Enum (..), Rational, negate, recip)

import Data.SBV
import Data.SBV.List qualified as L
import Data.SBV.Tuple

import Data.SBV.Either
import Data.SBV.List ((!!))
import Data.SBV.List qualified as L
import Data.SBV.String
-- import Data.SBV.Control

import Test.Tasty
import Test.Tasty.HUnit (Assertion, AssertionPredicable, assertFailure, testCase, (@?))

import GHC.Stack

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
        [ testCase "trueExample" $ checkThm =<< trueExample
        , testCase "falseExample" $ checkNotThm =<< falseExample
        , testCase "badDiv" $ checkNotSafe =<< test1
        , testCase "Can't divide by 0" $ checkIsSafe =<< test2
        , testCase "Can't divide by 0 alternative version" $ checkIsSafe =<< test3
        , testCase "Function is always positive" $ checkIsSafe =<< test4
        , testCase "Always returns true" $ checkIsSafe =<< test5
        , testCase "DeMorgan1 always returns true" $ checkIsSafe =<< test6]

-- | Generic assertion. This is less safe than usual, but will do.
assert :: AssertionPredicable t => t -> Assertion
assert t = t @? "assertion-failure"

-- | Turn provable to an assertion, theorem case
assertIsThm :: Provable a => a -> Assertion
assertIsThm t = assert (isTheorem t)

-- | Turn provable to a negative assertion, theorem case
assertIsntThm :: Provable a => a -> Assertion
assertIsntThm t = assert (fmap not (isTheorem t))

checkSat :: ThmResult -> Assertion
checkSat r = assert isThm
  where isThm = case r of
                  ThmResult Unsatisfiable{} -> return False :: IO Bool
                  ThmResult Satisfiable{}   -> return True
                  _                         -> error "checkSat: Unexpected result!"
checkThm :: ThmResult -> Assertion
checkThm r = assert isThm
  where isThm = case r of
                  ThmResult Unsatisfiable{} -> return True :: IO Bool
                  ThmResult Satisfiable{}   -> return False
                  _                         -> error "checkThm: Unexpected result!"

checkNotThm :: ThmResult -> Assertion
checkNotThm r = assert isThm
  where isThm = case r of
                  ThmResult Unsatisfiable{} -> return False
                  ThmResult Satisfiable{}   -> return True :: IO Bool
                  _                         -> error "checkThm: Unexpected result!"


checkIsSafe :: [SafeResult] -> Assertion
checkIsSafe xs = assert $ case filter (not . isSafe) xs of
                                 [] -> return True :: IO Bool
                                 xs -> return False

checkNotSafe :: [SafeResult] -> Assertion
checkNotSafe xs = assert $ case filter (not . isSafe) xs of
                                 [] -> return False
                                 xs -> return True :: IO Bool

trueExample :: IO ThmResult
trueExample = prove $ \x -> x `shiftL` 2 .== 4 * (x::SWord8)

falseExample :: IO ThmResult
falseExample = prove $ \x -> x `shiftL` 2 .== 2 * (x::SWord8)

-- | A simple variant of division, where we explicitly require the
-- caller to make sure the divisor is not 0.
checkedDiv :: (?loc :: CallStack) => SInt32 -> SInt32 -> SInt32
checkedDiv x y = sAssert (Just ?loc)
                         "Divisor should not be 0"
                         -- (y ./= 0)
                         (y ./= 0)
                         (x `sDiv` y)

test1 :: IO [SafeResult]
test1 = safe checkedDiv

test2 :: IO [SafeResult]
test2 = safe $ \x y -> ite (y .== 0) 3 (checkedDiv x y)

helpMe :: Int -> Either String Int
helpMe 0 = Right 0
helpMe _ = Left "not zero"

-- safeCallDiv :: SInt32 -> SInt32 -> SEither SString Int32
-- safeCallDiv x y = if y == 0 then sLeft "error" else sRight $ checkedDiv x y

safeCallDiv :: SInt32 -> SInt32 -> SInt32
safeCallDiv x y = ite (y .== 0) 0 (checkedDiv x y)

test3 :: IO [SafeResult]
test3 = safe safeCallDiv

incr :: (?loc :: CallStack) => SInt32 -> SInt32
incr x = sAssert (Just ?loc)
                "result should be positive"
                (0 .< x)
                x

incrImp :: SInt32 -> SInt32
incrImp x = incr (ite (x .< 0) (0 + 1) (x + 1))

test4 :: IO [SafeResult]
test4 = safe incrImp

isTrue :: (?loc :: CallStack) => SBool -> SBool
isTrue x = sAssert (Just ?loc)
                "result should return True"
                (x .== sTrue)
                x

ex1 :: SBool -> SBool
ex1 x = isTrue $ x .|| sNot x

test5 :: IO [SafeResult]
test5 = safe ex1

exDeMorgan1 :: SBool -> SBool -> SBool
exDeMorgan1 a b = isTrue $ sNot (a .|| b) .== (sNot a .&& sNot b)

test6 :: IO [SafeResult]
test6 = safe exDeMorgan1

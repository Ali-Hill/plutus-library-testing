{-@ LIQUID "--reflection"     @-}
{-@ LIQUID "--ple"            @-}
{-@ LIQUID "--no-termination" @-}
{-# LANGUAGE FlexibleContexts    #-}
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

import Test.Tasty
import Test.Tasty.HUnit (Assertion, AssertionPredicable, assertFailure, testCase, (@?))

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

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
        [ testCase "trueExample" $ checkThm =<< trueExample
        , testCase "falseExample" $ checkNotThm =<< falseExample]

trueExample :: IO ThmResult
trueExample = prove $ \x -> x `shiftL` 2 .== 4 * (x::SWord8)

falseExample :: IO ThmResult
falseExample = prove $ \x -> x `shiftL` 2 .== 2 * (x::SWord8)

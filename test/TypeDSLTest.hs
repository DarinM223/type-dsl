{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main (main) where

import Control.Monad.Identity
import Test.Tasty
import Test.Tasty.HUnit
import TypeDSL
import Base

testFunction :: Monad m => FnM "f" '["foo" >> 'VInt, "bar" >> 'VText] m 'VInt
testFunction = BindingDefault 2 :-> BindingDefault "bar" :-> FNil :=: \f -> do
  _ <- param @"foo" @'VInt f
  bar <- param @"bar" @'VText f
  _ <- define @"a" (ValueInt 2)
  _ <- define @"b" false
  call testFunction2 (pass @"bar" bar :> PNil)

testFunction2 :: Monad m => FnM "f2" '["foo" >> 'VInt, "bar" >> 'VText] m 'VInt
testFunction2 = BindingDefault 2 :-> Binding :-> FNil :=: \_ -> return 2

testSig1 :: Fn "f" '["foo" >> 'VInt, "bar" >> 'VText] 'VInt
testSig1 = BindingDefault 2 :-> Binding :-> FNil

testSig2 :: Fn "f" '["foo" >> 'VInt, "bar" >> 'VText] 'VInt
testSig2 = BindingDefault 2 :-> BindingDefault "bar" :-> FNil

matchTests :: TestTree
matchTests = testGroup "Match tests"
  [ testDefaultBinding
  , testBindingRequired
  ]

testDefaultBinding :: TestTree
testDefaultBinding =
  testCase "Passing required binding should match" $ do
    matches @_ @'["bar" >> 'VText] testSig1 @?= True
    matches @_ @'["foo" >> 'VInt, "bar" >> 'VText] testSig2 @?= True
    matches @_ @'[] testSig2 @?= True

testBindingRequired :: TestTree
testBindingRequired =
  testCase "Not passing in required bindings should fail" $ do
    matches @_ @'["foo" >> 'VInt] testSig1 @?= False
    matches @_ @'[] testSig1 @?= False

testCanCall :: TestTree
testCanCall = testCase "Test passing parameters to function" $ do
  canCall testSig1 (pass @"bar" (ValueText "hello") :> PNil) @?= True
  canCall testSig2 PNil @?= True
  canCall testSig1 (pass @"foo" (ValueInt 2) :> PNil) @?= False

testCall :: TestTree
testCall = testCase "Test calling function" $ do
  void $ call testFunction PNil
  void $ call testFunction2 (pass @"bar" (ValueText "") :> PNil)

allTests :: TestTree
allTests = testGroup "All tests"
  [ matchTests
  , testCanCall
  , testCall
  ]

main :: IO ()
main = defaultMain allTests

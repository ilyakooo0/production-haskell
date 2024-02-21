module Tests
  ( tests,
  )
where

import Task
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "Basic expression" $ do
      calculate "((1 + 2) * 3)" @=? Just 9
      calculate "1" @=? Just 1
      calculate "(((1 + 2) *3) +(1  +  1)   )" @=? Just 11
      calculate "(((1 + 2) *3) +(1  +  1)   " @=? Nothing
      calculate "1   + " @=? Nothing
      calculate "(1 / (8 - 8))" @=? Nothing
      calculate "(4 / 2)" @=? Just 2
  , testCase "Complex expression" $ do
      calculate "if ( 1 = 2 )  then (2 + 4)  else 2" @=? Just 2
      calculate "(((1 + 2) *3) +( if ( 1 = 2 )  then (2 + 4)  else 2  +  1)   )" @=? Just 12
      calculate "(((1 + 2) *3) +( if  1 = 2 )  then (2 + 4)  else 2  +  1)   )" @=? Nothing
      calculate "(((1 + 2) *3) +( if ( 1 = 2 )  then (2 + 4)  else 2  +  1)" @=? Nothing
      calculate "(((1 + 2) *3) +( f ( 1 = 2 )  then (2 + 4)  else 2  +  1)   )" @=? Nothing
      calculate "(((1 + 2) *3) +( f ( 1 = 2 )  then (2 + 4)  +  1)   )" @=? Nothing
  ]

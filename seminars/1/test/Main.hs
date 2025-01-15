module Main
  ( main,
  )
where

import Test.Tasty
import Test.Tasty.Hspec
import Tests

main :: IO ()
main = do
  tests' <- testSpecs tests
  defaultMain
    ( testGroup "tests" tests'
    )

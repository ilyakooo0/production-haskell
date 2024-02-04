module Main
  ( main,
  )
where

import Test.Tasty
import Tests

main :: IO ()
main = defaultMain tests

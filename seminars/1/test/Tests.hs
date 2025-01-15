module Tests
  ( tests,
  )
where

import Task
import Test.Hspec

tests :: Spec
tests = do
  it "regroup" $ do
    regroup (1 :: Int, (2 :: Int, 3 :: Int)) `shouldBe` (2, 1, 3)
    regroup ("1", ("2", "3")) `shouldBe` ("2", "1", "3")
    regroup (True, (False, True)) `shouldBe` (False, True, True)
  it "safeHead" $ do
    safeHead [1 :: Int, 2, 3] `shouldBe` Just 1
    safeHead [1294 :: Int, 2, 3, 4, 5, 6, 7, 7, 9] `shouldBe` Just 1294
    safeHead [] `shouldBe` (Nothing :: Maybe ())
  it "safeLast" $ do
    safeLast [1 :: Int, 2, 3] `shouldBe` Just 3
    safeLast [1294 :: Int, 2, 3, 4, 5, 6, 7, 7, 9] `shouldBe` Just 9
    safeLast [] `shouldBe` (Nothing :: Maybe ())
  it "withoutLast" $ do
    withoutLast [1 :: Int, 2, 3] `shouldBe` [1, 2]
    withoutLast [1294 :: Int, 2, 3, 4, 5, 6, 7, 7, 9] `shouldBe` [1294, 2, 3, 4, 5, 6, 7, 7]
    withoutLast [] `shouldBe` ([] :: [()])
  it "indexed" $ do
    indexed 2 [1 :: Int, 2, 3] `shouldBe` Just 3
    indexed 0 [1294 :: Int, 2, 3, 4, 5, 6, 7, 7, 9] `shouldBe` Just 1294
    indexed 1080 [1294 :: Int, 2, 3, 4, 5, 6, 7, 7, 9] `shouldBe` Nothing
    indexed 4 [1294 :: Int, 2, 3, 4, 5, 6, 7, 7, 9] `shouldBe` Just 5
    indexed 0 [] `shouldBe` (Nothing :: Maybe ())
    indexed 297 [] `shouldBe` (Nothing :: Maybe ())
  it "alternating" $ do
    take 10 alternating `shouldBe` [1, -2, 3, -4, 5, -6, 7, -8, 9, -10]
    alternating !! 42 `shouldBe` 43
    alternating !! 69 `shouldBe` -70
    alternating !! 420 `shouldBe` 421
  it "leftmost" $ do
    (leftmost . constructTree) [1 :: Int, 2, 3, 4, 5] `shouldBe` Just 4
    (leftmost . constructTree) [1 :: Int, 2, 3, 4] `shouldBe` Just 4
    (leftmost . constructTree) [1 :: Int, 2, 3, 99, 5] `shouldBe` Just 99
    (leftmost . constructTree) [8 :: Int, 7, 6, 5, 4, 1, 2, 3, 4, 5] `shouldBe` Just 3
  it "rightmost" $ do
    (rightmost . constructTree) [1 :: Int, 2, 3, 4, 5] `shouldBe` Just 3
    (rightmost . constructTree) [1 :: Int, 2, 3, 4] `shouldBe` Just 3
    (rightmost . constructTree) [1 :: Int, 2, 99, 5, 5] `shouldBe` Just 99
    (rightmost . constructTree) [8 :: Int, 7, 6, 5, 4, 1, 2, 3, 4, 5] `shouldBe` Just 2
  it "treeSum" $ do
    (treeSum . constructTree) [1, 2, 3, 4, 5] `shouldBe` sum [1, 2, 3, 4, 5]
    (treeSum . constructTree) [1, 2, 3, 4] `shouldBe` sum [1, 2, 3, 4]
    (treeSum . constructTree) [1, 2, 3, 5, 99] `shouldBe` sum [1, 2, 3, 5, 99]
    (treeSum . constructTree) [8, 7, 6, 5, 4, 1, 2, 3, 4, 5] `shouldBe` sum [8, 7, 6, 5, 4, 1, 2, 3, 4, 5]
    (treeSum . constructTree . take 100) [5 ..] `shouldBe` (sum . take 100) [5 ..]
    (treeSum . constructTree . take 100) [2 ..] `shouldBe` (sum . take 100) [2 ..]
  it "treeSize" $ do
    let test list = treeSize (constructTree list) `shouldBe` fromIntegral (length list)
    test $ replicate 11 ()
    test $ replicate 16 (8 :: Int)
    test [5, 2, 5, 7, 7, 3, 2, 7, 7 :: Int]
  it "insert" $ do
    constructTree [1 :: Int] `shouldNotBe` constructTree [1, 2]
    constructTree [1 :: Int] `shouldNotBe` constructTree []
    (insert 3 . insert 1 . insert 2 . constructTree) [] `shouldBe` constructTree [2, 1, 3]
    (insert 71 . insert 65 . insert 56 . insert 54 . insert 45 . insert 35 . insert 15 . insert 5 . constructTree) [50, 30, 60, 10, 55, 40, 70]
      `shouldBe` constructTree [50, 30, 60, 10, 55, 40, 70, 5, 54, 35, 65, 15, 56, 45, 71]
    (insert 71 . insert 54 . insert 45 . insert 35 . insert 15 . insert 65 . insert 56 . insert 5 . constructTree) [50, 30, 60, 10, 55, 40, 70]
      `shouldBe` constructTree [50, 30, 60, 10, 55, 40, 70, 5, 54, 35, 65, 15, 56, 45, 71]
    (insert 35 . insert 15 . insert 65 . insert 71 . insert 54 . insert 45 . insert 56 . insert 5 . constructTree) [50, 30, 60, 10, 55, 40, 70]
      `shouldBe` constructTree [50, 30, 60, 10, 55, 40, 70, 5, 54, 35, 65, 15, 56, 45, 71]
    (insert 35 . insert 54 . insert 45 . insert 56 . insert 5 . insert 15 . insert 65 . insert 71 . constructTree) [50, 30, 60, 10, 55, 40, 70]
      `shouldBe` constructTree [50, 30, 60, 10, 55, 40, 70, 5, 54, 35, 65, 15, 56, 45, 71]
    (insert 56 . insert 5 . insert 15 . insert 65 . insert 35 . insert 54 . insert 45 . insert 71 . constructTree) [50, 30, 60, 10, 55, 40, 70]
      `shouldBe` constructTree [50, 30, 60, 10, 55, 40, 70, 5, 54, 35, 65, 15, 56, 45, 71]
    (insert 56 . insert 56 . insert 5 . insert 5 . insert 15 . insert 65 . insert 35 . insert 54 . insert 45 . insert 71 . constructTree) [50, 30, 60, 10, 55, 40, 70]
      `shouldBe` constructTree [50, 30, 60, 10, 55, 40, 70, 5, 54, 35, 65, 15, 56, 45, 71]
  it "insertAll" $ do
    constructTree [1 :: Int] `shouldNotBe` constructTree [1, 2]
    constructTree [1 :: Int] `shouldNotBe` constructTree []
    (insertAll [2, 3, 1] . constructTree) [] `shouldBe` constructTree [2, 1, 3]
    (insertAll [71, 65, 56, 54, 45, 35, 15, 5] . constructTree) [50, 30, 60, 10, 55, 40, 70]
      `shouldBe` constructTree [50, 30, 60, 10, 55, 40, 70, 5, 54, 35, 65, 15, 56, 45, 71]
    (insertAll [71, 54, 45, 35, 15, 65, 56, 5] . constructTree) [50, 30, 60, 10, 55, 40, 70]
      `shouldBe` constructTree [50, 30, 60, 10, 55, 40, 70, 5, 54, 35, 65, 15, 56, 45, 71]
    (insertAll [35, 15, 65, 71, 54, 45, 56, 5] . constructTree) [50, 30, 60, 10, 55, 40, 70]
      `shouldBe` constructTree [50, 30, 60, 10, 55, 40, 70, 5, 54, 35, 65, 15, 56, 45, 71]
    (insertAll [35, 54, 45, 56, 5, 15, 65, 71] . constructTree) [50, 30, 60, 10, 55, 40, 70]
      `shouldBe` constructTree [50, 30, 60, 10, 55, 40, 70, 5, 54, 35, 65, 15, 56, 45, 71]
    (insertAll [56, 5, 15, 65, 35, 54, 45, 71] . constructTree) [50, 30, 60, 10, 55, 40, 70]
      `shouldBe` constructTree [50, 30, 60, 10, 55, 40, 70, 5, 54, 35, 65, 15, 56, 45, 71]
    (insertAll [56, 5, 15, 65, 35, 54, 45, 71, 5, 56, 35] . constructTree) [50, 30, 60, 10, 55, 40, 70]
      `shouldBe` constructTree [50, 30, 60, 10, 55, 40, 70, 5, 54, 35, 65, 15, 56, 45, 71]

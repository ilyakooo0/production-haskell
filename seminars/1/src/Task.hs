module Task
  ( regroup,
    safeHead,
    safeLast,
    withoutLast,
    indexed,
    alternating,
    constructTree,
    leftmost,
    rightmost,
    treeSum,
    insert,
    insertAll,
    treeSize,
    getInt, getString, twoAndHello
    , StringList
  )
where

import Numeric.Natural

data IntAndString = IntAndStringCons Int String

getInt :: IntAndString -> Int
getInt (IntAndStringCons int _) = int

getString :: IntAndString -> String
getString (IntAndStringCons _ string) = string

twoAndHello :: IntAndString
twoAndHello = IntAndStringCons 2 "Hello"

-- Implement a function which regroups a nested tuple as the types would suggest.
--
-- >>> regroup (8, ("Hello", True))
-- ("Hello", 8, True)
regroup :: (,) a ((,) b c) -> (,,) b a c
-- regroup :: (a, (b, c)) -> (b, a, c)
regroup (a, (b, c)) = (b, a, c)

data StringList = Cons String StringList | Nil
data List a = ListCons a (List a) | Nil'
--data [] a = (:) a ([] a) | []

oneTwoThree :: List Int
oneTwoThree = ListCons 1 (ListCons 2 (ListCons 3 Nil'))
-- oneTwoThree = (:) 1 ((:) 2 ((:) 3 []))
-- oneTwoThree = [1, 2, 3]

helloWorldList :: StringList
helloWorldList = Cons "Hello" (Cons "World" Nil)

-- data Maybe a = Just a | Nothing

stringListHead :: StringList -> Maybe String
stringListHead x = case x of
  Cons string string_list' -> Just string
  Nil -> Nothing
--   _     _     _         _          __  __
--  | |   (_)___| |_   ___| |_ _   _ / _|/ _|
--  | |   | / __| __| / __| __| | | | |_| |_
--  | |___| \__ \ |_  \__ \ |_| |_| |  _|  _|
--  |_____|_|___/\__| |___/\__|\__,_|_| |_|

-- Implement a function that returns the first element of a list, if there is one.
-- Hint: `Maybe a` is already defined as so:
--
--   data Maybe a = Just a | Nothing
--
-- If the list has any elements return `Just` the first element, else return `Nothing`.
--
-- >>> safeHead [1, 2, 3]
-- Just 1
--
-- >>> safeHead []
-- Nothing
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- Same as `safeHead`, but returns the last element.
--
-- >>> safeLast [1, 2, 3]
-- Just 3
--
-- >>> safeLast []
-- Nothing
safeLast :: [a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs

-- Returns the same list without the last element.
--
-- >>> withoutLast [1, 2, 3, 4]
-- [1, 2, 3]
--
-- >>> withoutLast []
-- []
withoutLast :: [a] -> [a]
withoutLast [] = []
withoutLast [x] = []
withoutLast (x:xs) = x:(withoutLast xs)

-- Returns the element at a given index if there is one.
-- Otherwise, return Nothing.
--
-- >>> indexed 2 [7, 6, 5, 4]
-- Just 5
--
-- >>> indexed 8 [6, 5, 4]
-- Nothing
indexed :: Natural -> [a] -> Maybe a
indexed 0 (x:_) = Just x
indexed number [] = Nothing
indexed number (_:xs) = indexed (number-1) xs

-- Is an infinite list, alternating the sign of every other element.
--
-- >>> alternating
-- [1, -2, 3, -4, 5, -6, ...
alternating :: [Integer]
alternating = cut rising
  where
    cut (x1:x2:xs) = x1:(-x2):cut xs
    rising = 1 : map (+ 1) rising
--   ____  _                          _
--  | __ )(_)_ __   __ _ _ __ _   _  | |_ _ __ ___  ___
--  |  _ \| | '_ \ / _` | '__| | | | | __| '__/ _ \/ _ \
--  | |_) | | | | | (_| | |  | |_| | | |_| | |  __/  __/
--  |____/|_|_| |_|\__,_|_|   \__, |  \__|_|  \___|\___|
--                            |___/

-- In this section you will implement what essentially is the Set from the
-- standard library. If you literally open the source code of the standard
-- library, it should look extremely similar to what you are doing here.

-- Define a binary tree structure.
--
-- This is a wiki page, but you probably shouldn't open it:
--   https://en.wikipedia.org/wiki/Binary_tree
--
-- Hint: a tree is either:
--   1. the end of the tree (an empty node with no elements)
--        (it should literally contain nothing)
--   2. a node which contains an element and two branches (left and right).
--        (in this context a 'branch' and a 'tree' is the same thing)
--
--   In case you didn't get it, a tree should have two constructors.
data Tree a = Branch a (Tree a) (Tree a) | EmptyBranch
  deriving (Show, Eq)

-- ^ Don't worry about this 'deriving' business. It is needed for tests
-- to compile. It will be explained later.

-- This next function is contrived but we need this since you can define
--   constructors however you want.
-- If you have trouble with this one, don't hesitate to ask for help.
--
-- Constructs a tree from the given elements.
-- The first element is the root of the tree and the rest of the elements
-- are the alternating left and right children.
--
-- You are expected to partition pluck the first element from the list and
--   split the rest into two lists, which you recursively pass to the same
--   function to get the left and right branches of the tree.
--
-- >>> constructTree [1, 2, 3]
--    1
--   / \
--  2   3
--
-- >>> constructTree [1, 2, 3, 4, 5, 6]
--         1
--        / \
--      2     3
--     / \   /
--    4   6 5
--
-- So every recursive step should look something like this:
-- [c, l, r, l, r, l, r, ...
--         c
--        / \
--      l     r
--     / \   / \
--    l   l r   r
--   .............
constructTree :: [a] -> Tree a
constructTree [] = EmptyBranch
constructTree (x:xs) = Branch x (constructTree leftBranch) (constructTree rightBranch)
  where
    split (a:b:xs) = (a:aTail, b:bTail)
      where
        (aTail, bTail) = split xs
    split [] = ([], [])
    split [x] = ([x], [])
    (leftBranch, rightBranch) = split xs

-- Returns the leftmost element of the given tree.
--
-- For this tree the result should be 2:
--    1
--   / \
--  2   3
--
-- For this tree the result should be 4:
--         1
--        / \
--      2     3
--     / \   /
--    4   6 5
leftmost :: Tree a -> Maybe a
leftmost EmptyBranch = Nothing
leftmost (Branch a EmptyBranch _) = Just a
leftmost (Branch _ br _) = leftmost br

-- Returns the rightmost element of the given tree.
--
-- For this tree the result should be 3:
--    1
--   / \
--  2   3
--
-- For this tree the result should be 3:
--         1
--        / \
--      2     3
--     / \   /
--    4   6 5
rightmost :: Tree a -> Maybe a
rightmost = error "TODO: rightmost"

-- Returns the sum of all elements in the tree
--
-- For this tree the result should be 6:
--    1
--   / \
--  2   3
--
-- For this tree the result should be 21:
--         1
--        / \
--      2     3
--     / \   /
--    4   6 5
treeSum :: Tree Integer -> Integer
treeSum = error "TODO: treeSum"

-- Returns the number of elements in the tree.
--
-- For this tree the result should be 3:
--    1
--   / \
--  2   3
--
-- For this tree the result should be 6:
--         1
--        / \
--      2     3
--     / \   /
--    4   6 5
treeSize :: Tree a -> Integer
treeSize = error "TODO: treeSize"

-- Insert the given integer into the tree, maintaining this property:
--   For every node in the tree all elements in the left subtree should be
--   smaller than the node element and all elements in the right subtree
--   should be greater or equal to the node element.
--
--    a      forall e in b. e < a
--   / \
--  b   c    forall e in c. e > a
--
-- Assume that the input tree already satisfies the property.
--
-- If the elements is already in the tree, replace the old element with the
--   new element. Thus, this function should not produce duplicates in the tree.
--
-- So, given the number 5 as input and this tree:
--    2
--   / \
--  1   3
-- the function should produce this tree:
--    2
--   / \
--  1   3
--       \
--        5
--
-- And given the number 2 as input and this tree:
--          7
--        /   \
--      2       9
--     / \     /
--    1   5   8
-- the function should produce this tree:
--          7
--        /   \
--      2       9
--     / \     /
--    1   5   8
--
-- And given the number 7 as input and this tree:
--          6
--        /   \
--      2       9
--     / \     /
--    1   5   8
-- the function should produce this tree:
--          6
--        /   \
--      2       9
--     / \     /
--    1   5   8
--           /
--          7
--
-- And given the number 3 as input and this tree:
--          7
--        /   \
--      2       9
--     / \     /
--    1   5   8
-- the function should produce this tree:
--          7
--        /   \
--      2       9
--     / \     /
--    1   5   8
--       /
--      3
--
-- Also, see comments for function insertAll for more examples of how this
-- function should behave.
insert :: Integer -> Tree Integer -> Tree Integer
insert = error "TODO: insert"

-- Insert all integers from the list into the given tree using the insert
-- function defined above.
--
-- >>> insertAll [1, 2, 3] <empty tree>
-- 1
--  \
--   2
--    \
--     3
--
-- >>> insertAll [2, 3, 1] <empty tree>
--    2
--   / \
--  1   3
--
-- >>> insertAll [3, 2, 1] <empty tree>
--     3
--    /
--   2
--  /
-- 1
--
-- >>> insertAll [8, 8, 10, 8] <empty tree>
-- 8
--  \
--   10
insertAll :: [Integer] -> Tree Integer -> Tree Integer
insertAll = error "TODO: insertAll"

module Tests
  ( tests,
  )
where

import Data.Foldable
import Data.Function
import Data.List
import Task
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "List Monoids" 
    [ testCase "FirstMonoid" $ do
      let getFirst = destructFirstMonoid . fold . fmap constructFirstMonoid
      getFirst [1 :: Int, 2, 3] @=? Just 1
      getFirst [] @=? (Nothing :: Maybe ())
      getFirst [True, False] @=? Just True
      getFirst [True, True, False] @=? Just True
      getFirst [1 .. 1000] @=? Just (1 :: Int)
      getFirst [100 .. 1000] @=? Just (100 :: Int)
      getFirst [6, 2, 7, 398, 1984, 383, -1020] @=? Just (6 :: Int)
    , testCase "LastMonoid" $ do
      let getLast = destructLastMonoid . fold . fmap constructLastMonoid
      getLast [1 :: Int, 2, 3] @=? Just 3
      getLast [] @=? (Nothing :: Maybe ())
      getLast [1 .. 1000] @=? Just (1000 :: Int)
      getLast [100 .. 10000] @=? Just (10000 :: Int)
      getLast [6, 2, 7, 398, 1984, 383, -1020, 498] @=? Just (498 :: Int)
      getLast [True, False] @=? Just False
      getLast [True, True, False] @=? Just False
      getLast [True, True, False, True] @=? Just True
    ]
  , testGroup "Todo List" $
    let createTasks :: [(String, Priority)] -> MyTaskManager
        createTasks = foldl (\t (a, b) -> createTask a b t) emptyTaskManager
        getPrioritySets :: MyTaskManager -> [[(Completion, String)]]
        getPrioritySets =
          fmap (sort . fmap (\(a, _, b) -> (a, b)))
            . groupBy ((==) `on` (\(_, p, _) -> p))
            . getPriorityList
    in
    [ testCase "emptyTaskManager" $
      getPriorityList (emptyTaskManager :: MyTaskManager) @=? []
    , testCase "createTask" $ do
      getPrioritySets
        ( createTasks
            [ ("a", Low),
              ("aaa", Medium),
              ("AAAAAAAAAA", High)
            ]
        )
        @=? [ [(NotCompleted, "AAAAAAAAAA")],
                     [(NotCompleted, "aaa")],
                     [(NotCompleted, "a")]
                   ]
      getPrioritySets
        ( createTasks
            [ ("a", Low),
              ("AAAAAAAAAA", High)
            ]
        )
        @=? [ [(NotCompleted, "AAAAAAAAAA")],
                     [(NotCompleted, "a")]
                   ]
      getPrioritySets
        ( createTasks
            [ ("a", Low),
              ("AAAAAAAAAA", High),
              ("aaa", Medium),
              ("oh HaiMark", Medium)
            ]
        )
        @=? [ [(NotCompleted, "AAAAAAAAAA")],
                     [(NotCompleted, "aaa"), (NotCompleted, "oh HaiMark")],
                     [(NotCompleted, "a")]
                   ]
      getPrioritySets
        ( createTasks
            [ ("a", Low),
              ("AAAAAAAAAA", High),
              ("aaa", Medium),
              ("oh HaiMark", Medium),
              ("OH NO", High)
            ]
        )
        @=? [ [(NotCompleted, "AAAAAAAAAA"), (NotCompleted, "OH NO")],
                     [(NotCompleted, "aaa"), (NotCompleted, "oh HaiMark")],
                     [(NotCompleted, "a")]
                   ]
    , testCase "toggleTaskCompletion" $ do
      getPrioritySets
        ( toggleTaskCompletion "AAAAAAAAAA" . createTasks $
            [ ("a", Low),
              ("AAAAAAAAAA", High),
              ("aaa", Medium),
              ("oh HaiMark", Medium),
              ("OH NO", High)
            ]
        )
        @=? [ [(Completed, "AAAAAAAAAA"), (NotCompleted, "OH NO")],
                     [(NotCompleted, "aaa"), (NotCompleted, "oh HaiMark")],
                     [(NotCompleted, "a")]
                   ]
      getPrioritySets
        ( toggleTaskCompletion "AAAAAAAAAA" . toggleTaskCompletion "AAAAAAAAAA" . createTasks $
            [ ("a", Low),
              ("AAAAAAAAAA", High),
              ("aaa", Medium),
              ("oh HaiMark", Medium),
              ("OH NO", High)
            ]
        )
        @=? [ [(NotCompleted, "AAAAAAAAAA"), (NotCompleted, "OH NO")],
                     [(NotCompleted, "aaa"), (NotCompleted, "oh HaiMark")],
                     [(NotCompleted, "a")]
                   ]
      getPrioritySets
        ( toggleTaskCompletion "a"
            . toggleTaskCompletion "not here"
            . toggleTaskCompletion "AAAAAAAAAA"
            . createTasks
            $ [ ("a", Low),
                ("AAAAAAAAAA", High),
                ("aaa", Medium),
                ("oh HaiMark", Medium),
                ("OH NO", High)
              ]
        )
        @=? [ [(Completed, "AAAAAAAAAA"), (NotCompleted, "OH NO")],
                     [(NotCompleted, "aaa"), (NotCompleted, "oh HaiMark")],
                     [(Completed, "a")]
                   ]
    , testCase "removeTask" $ do
      getPrioritySets
        ( removeTask "AAAAAAAAAA"
            . createTasks
            $ [ ("a", Low),
                ("AAAAAAAAAA", High),
                ("aaa", Medium),
                ("oh HaiMark", Medium),
                ("OH NO", High)
              ]
        )
        @=? [ [(NotCompleted, "OH NO")],
                     [(NotCompleted, "aaa"), (NotCompleted, "oh HaiMark")],
                     [(NotCompleted, "a")]
                   ]
      getPrioritySets
        ( removeTask "AAAAAAAAAA" . removeTask "AAAAAAAAAA"
            . createTasks
            $ [ ("a", Low),
                ("AAAAAAAAAA", High),
                ("aaa", Medium),
                ("oh HaiMark", Medium),
                ("OH NO", High)
              ]
        )
        @=? [ [(NotCompleted, "OH NO")],
                     [(NotCompleted, "aaa"), (NotCompleted, "oh HaiMark")],
                     [(NotCompleted, "a")]
                   ]
      getPrioritySets
        ( removeTask "aaa" . removeTask "oh HaiMark"
            . removeTask "AAAAAAAAAA"
            . removeTask "AAAAAAAAAA"
            . createTasks
            $ [ ("a", Low),
                ("AAAAAAAAAA", High),
                ("aaa", Medium),
                ("oh HaiMark", Medium),
                ("OH NO", High)
              ]
        )
        @=? [ [(NotCompleted, "OH NO")],
                     [(NotCompleted, "a")]
                   ]
    , testCase "modifyPriority" $ do
      getPrioritySets
        ( modifyPriority "aaa" High . createTasks $
            [ ("a", Low),
              ("AAAAAAAAAA", High),
              ("aaa", Medium),
              ("oh HaiMark", Medium),
              ("OH NO", High)
            ]
        )
        @=? [ [(NotCompleted, "AAAAAAAAAA"), (NotCompleted, "OH NO"), (NotCompleted, "aaa")],
                     [(NotCompleted, "oh HaiMark")],
                     [(NotCompleted, "a")]
                   ]
      getPrioritySets
        ( modifyPriority "aaa" High
            . toggleTaskCompletion "a"
            . modifyPriority "a" High
            . createTasks
            $ [ ("a", Low),
                ("AAAAAAAAAA", High),
                ("aaa", Medium),
                ("oh HaiMark", Medium),
                ("OH NO", High)
              ]
        )
        @=? [ [ (Completed, "a"),
                       (NotCompleted, "AAAAAAAAAA"),
                       (NotCompleted, "OH NO"),
                       (NotCompleted, "aaa")
                     ],
                     [(NotCompleted, "oh HaiMark")]
                   ]
      getPrioritySets
        ( modifyPriority "aaa" High
            . modifyPriority "a" High
            . modifyPriority "a" High
            . modifyPriority "oh HaiMark" High
            . createTasks
            $ [ ("a", Low),
                ("AAAAAAAAAA", High),
                ("aaa", Medium),
                ("oh HaiMark", Medium),
                ("OH NO", High)
              ]
        )
        @=? [ [ (NotCompleted, "AAAAAAAAAA"),
                       (NotCompleted, "OH NO"),
                       (NotCompleted, "a"),
                       (NotCompleted, "aaa"),
                       (NotCompleted, "oh HaiMark")
                     ]
                   ]
      getPrioritySets
        ( modifyPriority "aaa" High
            . modifyPriority "a" High
            . modifyPriority "ntestCase here sihji" High
            . modifyPriority "oh HaiMark" Low
            . modifyPriority "a" High
            . modifyPriority "oh HaiMark" High
            . createTasks
            $ [ ("a", Low),
                ("AAAAAAAAAA", High),
                ("aaa", Medium),
                ("oh HaiMark", Medium),
                ("OH NO", High)
              ]
        )
        @=? [ [ (NotCompleted, "AAAAAAAAAA"),
                       (NotCompleted, "OH NO"),
                       (NotCompleted, "a"),
                       (NotCompleted, "aaa")
                     ],
                     [(NotCompleted, "oh HaiMark")]
                   ]
    , testCase "renameTask" $ do
      getPrioritySets
        ( renameTask "AAAAAAAAAA" "henlo"
            . createTasks
            $ [ ("a", Low),
                ("AAAAAAAAAA", High),
                ("aaa", Medium),
                ("oh HaiMark", Medium),
                ("OH NO", High)
              ]
        )
        @=? [ [(NotCompleted, "OH NO"), (NotCompleted, "henlo")],
                     [(NotCompleted, "aaa"), (NotCompleted, "oh HaiMark")],
                     [(NotCompleted, "a")]
                   ]
      getPrioritySets
        ( renameTask "AAAAAAAAAA" "henlo"
            . toggleTaskCompletion "a"
            . renameTask "oh HaiMark" "iz task"
            . createTasks
            $ [ ("a", Low),
                ("AAAAAAAAAA", High),
                ("aaa", Medium),
                ("oh HaiMark", Medium),
                ("OH NO", High)
              ]
        )
        @=? [ [(NotCompleted, "OH NO"), (NotCompleted, "henlo")],
                     [(NotCompleted, "aaa"), (NotCompleted, "iz task")],
                     [(Completed, "a")]
                   ]
      getPrioritySets
        ( renameTask "AAAAAAAAAA" "henlo"
            . toggleTaskCompletion "iz task"
            . renameTask "oh HaiMark" "iz task"
            . renameTask "iz not here" "ajajaj"
            . createTasks
            $ [ ("a", Low),
                ("AAAAAAAAAA", High),
                ("aaa", Medium),
                ("oh HaiMark", Medium),
                ("OH NO", High)
              ]
        )
        @=? [ [(NotCompleted, "OH NO"), (NotCompleted, "henlo")],
                     [(Completed, "iz task"), (NotCompleted, "aaa")],
                     [(NotCompleted, "a")]
                   ]
    ]
  , testGroup "Coffee"
    [ testCase "HasPrice Coffee" $ do
      price (Coffee 2 [OatMilk, Cinnamon]) @=? 2 + price OatMilk + price Cinnamon
      price (Coffee 2 [Cinnamon, Cinnamon]) @=? 2 + price Cinnamon + price Cinnamon
      price (Coffee 2 []) @=? 2
      price (Coffee 2 [Cinnamon, Cream, Cinnamon])
        @=? 2
        + price Cinnamon
        + price Cinnamon
        + price Cream
    , testCase "chargeCoffee" $ do
      chargeCoffee (Coffee 2 [OatMilk, Cinnamon]) (DebitCard 199 69)
        @=? Just (DebitCard 112 69)
      chargeCoffee (Coffee 2 [OatMilk, Cinnamon]) (DebitCard 87 69)
        @=? Just (DebitCard 0 69)
      chargeCoffee (Coffee 2 [OatMilk, Cinnamon]) (DebitCard 86 69)
        @=? Nothing
      chargeCoffee (Coffee 2 [OatMilk, Cinnamon]) (DebitCard 0 69)
        @=? Nothing
      chargeCoffee (Coffee 2 [OatMilk, Cinnamon]) (DebitCard (-2189) 69)
        @=? Nothing
      chargeCoffee (Coffee 2 [OatMilk, Cinnamon, WhiteSugar]) (DebitCard 199 69)
        @=? Just (DebitCard 97 69)
      chargeCoffee (Coffee 2 [WhiteSugar, OatMilk, Cinnamon, WhiteSugar]) (DebitCard 199 69)
        @=? Just (DebitCard 82 69)
    , let coffee = Coffee 2 [OatMilk, Cinnamon]
      in testCase "payForCoffee" $ do
        payForCoffee (Customer [DebitCard 199 69] 0) coffee
          @=? Just (Card (DebitCard 112 69))
        payForCoffee (Customer [DebitCard 86 12, DebitCard 199 69] 2) coffee
          @=? Just (Card (DebitCard 112 69))
        payForCoffee (Customer [DebitCard (-12) 8, DebitCard 86 12, DebitCard 199 69] 0) coffee
          @=? Just (Card (DebitCard 112 69))
        payForCoffee (Customer [DebitCard 87 8, DebitCard 111 12, DebitCard 199 69] 0) coffee
          @=? Just (Card (DebitCard 0 8))
        payForCoffee (Customer [DebitCard 86 8, DebitCard 86 12, DebitCard 82 69] 0) coffee
          @=? Nothing
        payForCoffee (Customer [DebitCard 86 8, DebitCard 86 12, DebitCard 82 69] 87) coffee
          @=? Just (Cash 0)
        payForCoffee (Customer [DebitCard 86 8, DebitCard 86 12, DebitCard 81 69] 86) coffee
          @=? Nothing
    , testCase "applyPayment" $ do
      applyPayment (Customer [DebitCard 199 69] 0) (Card (DebitCard 112 69))
        @=? Customer [DebitCard 112 69] 0
      applyPayment (Customer [DebitCard 111 12, DebitCard 199 69] 2) (Card (DebitCard 112 69))
        @=? Customer [DebitCard 111 12, DebitCard 112 69] 2
      applyPayment (Customer [DebitCard (-12) 8, DebitCard 111 12, DebitCard 199 69] 0) (Card (DebitCard 112 69))
        @=? Customer [DebitCard (-12) 8, DebitCard 111 12, DebitCard 112 69] 0
      applyPayment (Customer [DebitCard 112 8, DebitCard 111 12, DebitCard 199 69] 0) (Card (DebitCard 0 8))
        @=? Customer [DebitCard 0 8, DebitCard 111 12, DebitCard 199 69] 0
      applyPayment (Customer [DebitCard 111 8, DebitCard 111 12, DebitCard 110 69] 112) (Cash 0)
        @=? Customer [DebitCard 111 8, DebitCard 111 12, DebitCard 110 69] 0
      applyPayment (Customer [DebitCard 111 8, DebitCard 111 12, DebitCard 110 69] 112) (Card (DebitCard 0 13))
        @=? Customer [DebitCard 111 8, DebitCard 111 12, DebitCard 110 69] 112
    , let coffee = Coffee 2 [OatMilk, Cinnamon]
      in testCase "buyCoffee" $ do
        buyCoffee (Customer [DebitCard 199 69] 0) coffee
          @=? Just (Customer [DebitCard 112 69] 0)
        buyCoffee (Customer [DebitCard 86 12, DebitCard 199 69] 2) coffee
          @=? Just (Customer [DebitCard 86 12, DebitCard 112 69] 2)
        buyCoffee (Customer [DebitCard (-12) 8, DebitCard 86 12, DebitCard 199 69] 0) coffee
          @=? Just (Customer [DebitCard (-12) 8, DebitCard 86 12, DebitCard 112 69] 0)
        buyCoffee (Customer [DebitCard 87 8, DebitCard 86 12, DebitCard 199 69] 0) coffee
          @=? Just (Customer [DebitCard 0 8, DebitCard 86 12, DebitCard 199 69] 0)
        buyCoffee (Customer [DebitCard 86 8, DebitCard 86 12, DebitCard 84 69] 0) coffee
          @=? Nothing
        buyCoffee (Customer [DebitCard 86 8, DebitCard 86 12, DebitCard 82 69] 87) coffee
          @=? Just (Customer [DebitCard 86 8, DebitCard 86 12, DebitCard 82 69] 0)
        buyCoffee (Customer [DebitCard 86 8, DebitCard 86 12, DebitCard 81 69] 86) coffee
          @=? Nothing
    , testCase "saveTheDiabetic" $ do
      saveTheDiabetic
        [ Coffee 0 [WhiteSugar, BrownSugar, Cinnamon],
          Coffee 0 [WhiteSugar, BrownSugar, Cinnamon],
          Coffee 0 [WhiteSugar, BrownSugar, Cinnamon]
        ]
        @=? ( 150,
                     [ Coffee 0 [Cinnamon],
                       Coffee 0 [Cinnamon],
                       Coffee 0 [Cinnamon]
                     ]
                   )
      saveTheDiabetic
        [ Coffee 0 [WhiteSugar, BrownSugar, Cinnamon, BrownSugar, SoyMilk],
          Coffee 0 [WhiteSugar, BrownSugar, WhiteSugar, WhiteSugar, Cinnamon],
          Coffee 0 [WhiteSugar, BrownSugar, Cinnamon]
        ]
        @=? ( 215,
                     [ Coffee 0 [Cinnamon, SoyMilk],
                       Coffee 0 [Cinnamon],
                       Coffee 0 [Cinnamon]
                     ]
                   )
      saveTheDiabetic
        [ Coffee 0 [Cinnamon, SoyMilk],
          Coffee 0 [Cinnamon],
          Coffee 0 [Cinnamon],
          Coffee 69 []
        ]
        @=? ( 0,
                     [ Coffee 0 [Cinnamon, SoyMilk],
                       Coffee 0 [Cinnamon],
                       Coffee 0 [Cinnamon],
                       Coffee 69 []
                     ]
                   )
      saveTheDiabetic
        []
        @=? ( 0,
                     []
                   )
    , testCase "calculateSugarDanger" $ do
      calculateSugarDanger
        [ Coffee 0 [WhiteSugar, BrownSugar, Cinnamon],
          Coffee 0 [WhiteSugar, BrownSugar, Cinnamon],
          Coffee 0 [WhiteSugar, BrownSugar, Cinnamon]
        ]
        @=? 9
      calculateSugarDanger
        [ Coffee 0 [WhiteSugar, BrownSugar, Cinnamon, BrownSugar, SoyMilk],
          Coffee 0 [WhiteSugar, BrownSugar, WhiteSugar, WhiteSugar, Cinnamon],
          Coffee 0 [WhiteSugar, BrownSugar, Cinnamon],
          Coffee 0 []
        ]
        @=? 14
      calculateSugarDanger
        [ Coffee 0 [Cinnamon, SoyMilk],
          Coffee 0 [Cinnamon],
          Coffee 0 [Cinnamon]
        ]
        @=? 0
      calculateSugarDanger
        []
        @=? 0
    ]
  ]

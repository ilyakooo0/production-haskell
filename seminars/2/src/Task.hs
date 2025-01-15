module Task
  ( -- * List Monoids
    constructFirstMonoid,
    destructFirstMonoid,
    FirstMonoid,
    constructLastMonoid,
    destructLastMonoid,
    LastMonoid,

    -- * Todo List
    Priority (..),
    Completion (..),
    TaskManager (..),
    MyTaskManager,

    -- * Coffee
    DebitCard (..),
    Coffee (..),
    CoffeeExtra (..),
    HasPrice (..),
    chargeCoffee,
    PaymentMethod (..),
    Customer (..),
    payForCoffee,
    applyPayment,
    buyCoffee,
    saveTheDiabetic,
    calculateSugarDanger,
  )
where

--   _     _     _     __  __                   _     _
--  | |   (_)___| |_  |  \/  | ___  _ __   ___ (_) __| |___
--  | |   | / __| __| | |\/| |/ _ \| '_ \ / _ \| |/ _` / __|
--  | |___| \__ \ |_  | |  | | (_) | | | | (_) | | (_| \__ \
--  |_____|_|___/\__| |_|  |_|\___/|_| |_|\___/|_|\__,_|___/

-- Write a datatype and a Monoid instance for it such that:
--
--   (destructFirstMonoid . fold . fmap constructFirstMonoid) == safeHead
--
-- (constructFirstMonoid and destructFirstMonoid would wrap and
-- unwrap the `FirstMonoid`)
--
-- In other words, it needs to have a monoid instance such that `fold`ing
-- a list of `FirstMonoid`s should return the first element
--
-- >>> (destructFirstMonoid . fold . fmap constructFirstMonoid) [1, 2, 3]
-- Just 1
--
-- >>> (destructFirstMonoid . fold . fmap constructFirstMonoid) []
-- Nothing
data FirstMonoid a

instance Semigroup (FirstMonoid a)

instance Monoid (FirstMonoid a)

-- Warps an `a` in a `FirstMonoid`.
constructFirstMonoid :: a -> FirstMonoid a
constructFirstMonoid = error "TODO: constructFirstMonoid"

-- Unwraps the `a` from a `FirstMonoid`, if there is one.
destructFirstMonoid :: FirstMonoid a -> Maybe a
destructFirstMonoid = error "TODO: destructFirstMonoid"

-- Write a datatype and a Monoid instance for it such that:
--
--   (destructLastMonoid . fold . fmap constructLastMonoid) == safeLast
--
-- (constructLastMonoid and destructLastMonoid would wrap and
-- unwrap the `LastMonoid`)
--
-- In other words, it needs to have a monoid instance such that `fold`ing
-- a list of `LastMonoid`s should return the last element
--
-- >>> (destructLastMonoid . fold . fmap constructLastMonoid) [1, 2, 3]
-- Just 3
--
-- >>> (destructLastMonoid . fold . fmap constructLastMonoid) []
-- Nothing
data LastMonoid a

instance Semigroup (LastMonoid a)

instance Monoid (LastMonoid a)

-- Warps an `a` in a `LastMonoid`.
constructLastMonoid :: a -> LastMonoid a
constructLastMonoid = error "TODO: constructLastMonoid"

-- Unwraps the `a` from a `LastMonoid`, if there is one.
destructLastMonoid :: LastMonoid a -> Maybe a
destructLastMonoid = error "TODO: destructLastMonoid"

--   _____         _         _     _     _
--  |_   _|__   __| | ___   | |   (_)___| |_
--    | |/ _ \ / _` |/ _ \  | |   | / __| __|
--    | | (_) | (_| | (_) | | |___| \__ \ |_
--    |_|\___/ \__,_|\___/  |_____|_|___/\__|

-- Your task is to create a todo list backend, which supports adding tasks,
-- changing the priority of tasks, completing tasks, renaming tasks, and
-- displaying tasks as a list in order of descending priority.

-- | The priority of a task.
data Priority = Low | Medium | High
  deriving (Show, Eq)

-- | The completion status of a task.
data Completion = Completed | NotCompleted
  deriving (Show, Eq, Ord)

class TaskManager t where
  -- | Returns an empty task manager backend (it should have no tasks).
  emptyTaskManager :: t

  -- | Gets the list of all tasks, sorted by priority in the order:
  -- High, Medium, Low
  --
  -- Ordering of tasks with the same priority is not defined
  -- (You can do whatever you want)
  getPriorityList :: t -> [(Completion, Priority, String)]

  -- | Creates a new task with the given name and priority.
  -- Tasks are uniquely identified by their name.
  -- When a task is newly created its status should be 'NotCompleted'.
  --
  -- If there is already a task with the given name, then the
  -- behavior is undefined. (You can do whatever you want)
  createTask :: String -> Priority -> t -> t

  -- | Toggles a task with the given name.
  -- If the task has the status 'Completed', then it would become 'NotCompleted'.
  -- If the task has the status 'NotCompleted', then it would become 'Completed'.
  --
  -- Needless to say, toggling the same task twice should be
  -- equivalent to not doing anything.
  --
  -- If there is no tasks with the given name, then it does nothing.
  toggleTaskCompletion :: String -> t -> t

  -- | Removes the task with the given name.
  -- If there isn't a task with the given name, then it does nothing.
  removeTask :: String -> t -> t

  -- | Sets the priority of the task with the given name.
  -- If there isn't a task with the given name, then it does nothing.
  modifyPriority :: String -> Priority -> t -> t

  -- | Renames the task with the given name to have a new name.
  -- The first argument is the old name, the second argument is the new name.
  -- If there isn't a task with the given name, then it does nothing.
  -- If the new name clashes with an existing task, then the
  -- behavior is undefined. (You can do whatever you want)
  renameTask :: String -> String -> t -> t

-- The task manager you are creating.
data MyTaskManager

-- TODO: You have to make your 'MyTaskManager' an instance of
-- the 'TaskManager' typeclass.
instance TaskManager MyTaskManager

--    ____       __  __
--   / ___|___  / _|/ _| ___  ___
--  | |   / _ \| |_| |_ / _ \/ _ \
--  | |__| (_) |  _|  _|  __/  __/
--   \____\___/|_| |_|  \___|\___|
--
--      /~~~~~~~~~~~~~~~~~~~/|
--     /              /######/ / |
--    /              /______/ /  |
--   ========================= /||
--   |_______________________|/ ||
--    |  \****/     \__,,__/    ||
--    |===\**/       __,,__     ||
--    |______________\====/%____||
--    |   ___        /~~~~\ %  / |
--   _|  |===|===   /      \%_/  |
--  | |  |###|     |########| | /
--  |____\###/______\######/__|/
--  ~~~~~~~~~~~~~~~~~~~~~~~~~~

-- | Just a typealias to make types more readable.
type RUBAmount = Integer

-- | Represents a cup of coffee with extra ingredients added.
data Coffee
  = Coffee
      { -- | The price of the coffee itself
        -- (without the cost of the extra ingredients.)
        cost :: RUBAmount,
        -- | The extra ingredients added to the coffee.
        extras :: [CoffeeExtra]
      }
  deriving (Eq, Show)

-- Extra ingredients added to the coffee.
data CoffeeExtra
  = Cream
  | AlmondMilk
  | SoyMilk
  | OatMilk
  | Cinnamon
  | WhiteSugar
  | BrownSugar
  deriving (Eq, Show)

-- | A typeclass representing objects, for which the price can be calculated.
class HasPrice x where
  -- | Calculated the price of the item.
  price :: x -> RUBAmount

-- I have already made a 'CoffeeExtra' an instance of 'HasPrice' for you.
-- You should use this in price calculations.
instance HasPrice CoffeeExtra where
  price Cream = 50
  price AlmondMilk = 70
  price SoyMilk = 80
  price OatMilk = 65
  price Cinnamon = 20
  price WhiteSugar = 15
  price BrownSugar = 35

-- TODO: You want to make 'Coffee' an instance of 'HasPrice'.
instance HasPrice Coffee

-- | A debit card.
-- Note: the balance can be negative.
data DebitCard
  = DebitCard
      { balance :: RUBAmount,
        cardId :: Int
      }
  deriving (Eq, Show)

-- | Given a coffee and a debit card, deduce the total price of the coffee from
-- the debit card. Return the given debit card with the reduced balance.
--
-- If the balance of the debit card after deducing the price of the coffee
-- would become negative, return 'Nothing' from the function.
chargeCoffee :: Coffee -> DebitCard -> Maybe DebitCard
chargeCoffee = error "TODO: chargeCoffee"

-- | How was the coffee charged.
data PaymentMethod
  = -- | The coffee was charged using a debit card.
    Card
      { -- | The debit card with the balance deduced.
        chargedCard :: DebitCard
      }
  | -- | The coffee was charged with cash.
    Cash
      { -- | The leftover money from the payment.
        -- (We assume that the person pays with all of the cash he has.)
        change :: RUBAmount
      }
  deriving (Eq, Show)

-- | Represents a coffee shop customer with all of the money he has on him.
data Customer
  = Customer
      { -- | All of the debit hards the person has on him.
        -- Sorted in order of descending preference.
        -- (He would prefer to use the first card in the list.)
        cards :: [DebitCard],
        -- | The amount of cash the customer has on him.
        cash :: RUBAmount
      }
  deriving (Eq, Show)

-- You have some new privacy-invading piece of ... technology, which
-- automatically scans the customer, determines how he would prefer to pay and
-- charges him automatically.

-- | This function determines how the given customer would prefer to pay for the
-- coffee he wants.
--
-- The customer would always prefer to pay with a debit card if it is possible.
-- The customer has a strict preference in the cards he carries with him:
-- he would always like to pay with the first card in his wallet (the list). If
-- the first card doesn't have enough money on it, then he would prefer to use
-- the second card, and so on.
--
-- If non of the cards have enough balance to cover the coffee, then the
-- customer will have to pay in cash.
--
-- NOTE: The customer can only buy a coffee with strictly one payment method.
--   (Only one card or cash)
payForCoffee :: Customer -> Coffee -> Maybe PaymentMethod
payForCoffee = error "TODO: payForCoffee"

-- | This function should apply the chosen payment method to the customer.
-- In other words, it needs to apply the charge to the customer himself
-- (update the debit card or update the amount of cash he currently has)
--
-- If the specified card is not present in the 'Customer' structure, then
-- do nothing.
--
-- NOTE: You can not change the order of the cards.
applyPayment :: Customer -> PaymentMethod -> Customer
applyPayment = error "TODO: applyPayment"

-- | Performs the full payment (as in 'payForCoffee') and
-- returns the modified customer (as in 'applyPayment').
buyCoffee :: Customer -> Coffee -> Maybe Customer
buyCoffee = error "TODO: buyCoffee"

-- | You know that due to a medical condition the customer needs to watch his
-- sugar intake. The new privacy-invading piece of ... technology can now
-- automatically apply filters to coffee orders.
--
-- You have to make a filter, which removes any occurrences of sugar
-- (either WhiteSugar or BrownSugar) from the ordered cups of coffee and
-- return the filtered orders and the amount of money the customer saves by
-- being healthy (sugar costs money after all). So, you also have to count how
-- much the total sugar would have cost.
--
-- NOTE: You can not change the order of the coffee.
saveTheDiabetic :: [Coffee] -> (RUBAmount, [Coffee])
saveTheDiabetic = error "TODO: saveTheDiabetic"

-- | And just to torment those who love sugar lets make a function which
-- calculates health hazard (sugar content) of the given orders.
--
-- The only ingredient that elevates the danger level is, of course, sugar:
--
--   WhiteSugar: 2 extra danger points
--   BrownSugar: 1 extra danger point
calculateSugarDanger :: [Coffee] -> Int
calculateSugarDanger = error "TODO: calculateSugarDanger"

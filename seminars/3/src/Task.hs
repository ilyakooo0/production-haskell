module Task
  ( Tape (..),
    touchTape,
    initializeTape,
    emptyTape,
    shiftTapeL,
    shiftTapeR,
    ErrorState (..),
    executeErrorState,
    runErrorState,
    get,
    put,
    throwError,
    modify,
    BFState (..),
    BFError (..),
    BFMonad,
    readInput,
    writeOutput,
    shiftDataR,
    shiftDataL,
    readData,
    writeData,
    BFCommand (..),
    executeCommand,
    evaluateProgram,
    executeProgram,
  )
where

import Data.Char
import Data.List
import Control.Monad.Except
import Control.Monad.State

--   ____  _____   _       _                           _
--  | __ )|  ___| (_)_ __ | |_ ___ _ __ _ __  _ __ ___| |_ ___ _ __
--  |  _ \| |_    | | '_ \| __/ _ \ '__| '_ \| '__/ _ \ __/ _ \ '__|
--  | |_) |  _|   | | | | | ||  __/ |  | |_) | | |  __/ ||  __/ |
--  |____/|_|     |_|_| |_|\__\___|_|  | .__/|_|  \___|\__\___|_|
--                                     |_|

-- In this section you will implements an interpreter for the BF language.
-- If you are not familiar with it, please familiarize yourself:
--   https://en.wikipedia.org/wiki/Brainfuck

--   _____
--  |_   _|_ _ _ __  ___
--    | |/ _` | '_ \/ -_)
--    |_|\__,_| .__/\___|
--            |_|

-- | Represents a possibly infinite tape with a tape head pointing to one of the
-- tape value.
--
-- So, this object:
--
-- Tape
--  { leftTape :: [2, 6, ...],
--    tapeValue :: 1,
--    rightTape :: [5, 8, ...]
--  }
--
--  Would represent this tape:
--
--   ◀──┬─────┬─────┬─────┬─────┬─────┬──▶
--  ... │  6  │  2  │  1  │  5  │  8  │ ...
--   ◀──┴─────┴─────┴─────┴─────┴─────┴──▶
--                     ▲
--                     │
data Tape a
  = Tape
      { leftTape :: [a],
        tapeValue :: a,
        rightTape :: [a]
      }
  deriving (Eq, Ord)

-- We need a special instance to handle infinite tapes. The derived
-- implementation would go into an infinite loop.
instance Show a => Show (Tape a) where
  show (Tape l c right) =
    "["
      <> tapeify (reverse (take 5 l))
      <> " { "
      <> show c
      <> " } "
      <> tapeify (take 5 right)
      <> "]"
    where
      tapeify = mconcat . intersperse " " . fmap show

-- | Creates a tape from a list.
--
-- Note that the tape will be empty to the left.
--
-- >>> initializeTape [1, 5, 8, ...]
--
--  ┌─────┬─────┬─────┬──▶
--  │  1  │  5  │  8  │ ...
--  └─────┴─────┴─────┴──▶
--     ▲
--     │
initializeTape :: [a] -> Maybe (Tape a)
initializeTape [] = Nothing
initializeTape (a : aa) = Just (Tape [] a aa)

-- | This is represents the operation of moving the tape pointer to the left.
shiftTapeL :: Tape a -> Maybe (Tape a)
shiftTapeL (Tape (l : ll) c rr) = Just (Tape ll l (c : rr))
shiftTapeL _ = Nothing

-- | This is represents the operation of moving the tape pointer to the right.
shiftTapeR :: Tape a -> Maybe (Tape a)
shiftTapeR (Tape ll c (r : rr)) = Just (Tape (c : ll) r rr)
shiftTapeR _ = Nothing

-- | This operation allows you to modify the cell at the tape pointer.
touchTape :: (a -> a) -> Tape a -> Tape a
touchTape f (Tape l c r) = Tape l (f c) r

--   ___ _        _                                _
--  / __| |_ __ _| |_ ___   _ __  ___ _ _  __ _ __| |
--  \__ \  _/ _` |  _/ -_) | '  \/ _ \ ' \/ _` / _` |
--  |___/\__\__,_|\__\___| |_|_|_\___/_||_\__,_\__,_|

-- In this section we will implement the state monad with some basic operations
-- it supports.

-- | Since the standard `State` monad doesn't have error handling capabilities.
-- we will implement a version of `State`, which can also handle errors.
--
-- `e` is the type of the possible error
-- `s` is the type of the state that is carried through the monad
-- `a` is the value that is returned.
-- data ErrorState e s a
--   = ErrorState
--       {runErrorState :: s -> Either e (a, s)}

type ErrorState e s a = ExceptT e (State s) a

-- | Executes the state monad and returns the result without returning the
-- final state.
executeErrorState :: s -> ErrorState e s a -> Either e a
executeErrorState s = flip evalState s . runExceptT

-- runErrorState :: s -> ErrorState e s a -> Either e s
runErrorState s = flip runState s . runExceptT

--   ___ ___                          _
--  | _ ) __|  _ __  ___ _ _  __ _ __| |
--  | _ \ _|  | '  \/ _ \ ' \/ _` / _` |
--  |___/_|   |_|_|_\___/_||_\__,_\__,_|

-- In this section we will implement the specific state we will be using and
-- some operations that work within it.

-- | This represents the state of our BF interpreter.
data BFState
  = BFState
      { -- | The data tape.
        bfDataTape :: Tape Int,
        -- | The input stream of the BF interpreter.
        bfInput :: [Int],
        -- | The output stream of the BF interpreter.
        bfOutput :: [Int]
      }
  deriving (Eq, Show)

-- | This represents the errors that can occur while executing a BF program.
data BFError
  = -- | Tried reading input, but the input stream was empty.
    NotEnoughInput
  | -- | Tried going past the end of the data tape (if it was finite).
    DataTapeExhausted
  deriving (Show, Eq)

-- | The monad in which we will interpreting the BF commands.
type BFMonad a = ErrorState BFError BFState a

-- | This operation should consume one element from the input stream
-- and return it.
--
-- If there is not enough input, it should throw 'NotEnoughInput' error.
readInput :: BFMonad Int
readInput = do
  bfst <- get
  case bfInput bfst of
    [] -> throwError NotEnoughInput
    (x:xs)-> do
      put bfst { bfInput = xs }
      return x

-- | This operation should write one element to the output stream.
-- This should just prepend the character to the start of the string using the
-- `:` (cons) operator.
--
-- This will make writing O(1).
writeOutput :: Int -> BFMonad ()
writeOutput value = do
  bfst <- get 
  let old_output = bfOutput bfst
  put bfst {bfOutput = value : old_output }
  return ()

-- | This operation shifts the data pointer to the right.
--
-- NOTE: if the tape has ended, you should throw the 'DataTapeExhausted' error.
shiftDataR :: BFMonad ()
shiftDataR = shiftData shiftTapeR


shiftData :: (Tape Int -> Maybe (Tape Int)) -> BFMonad ()
shiftData f = do
  bfst <- get
  case f $ bfDataTape bfst of
    Nothing -> throwError DataTapeExhausted
    Just x -> put bfst {bfDataTape = x }


-- | This operation shifts the data pointer to the left.
--
-- NOTE: if the tape has ended, you should throw the 'DataTapeExhausted' error.
shiftDataL :: BFMonad ()
shiftDataL = shiftData shiftTapeL

-- | This operation reads the element at the data pointer.
readData :: BFMonad Int
readData = do
  bfst <- get
  return (tapeValue $ bfDataTape bfst)

-- | This operation writes the element to the current data pointer.
writeData :: Int -> BFMonad ()
writeData val = do
  bfst <- get
  put bfst {bfDataTape = touchTape (const val) $ bfDataTape bfst }
--   _____ _          _     _                        _
--  |_   _| |_  ___  (_)_ _| |_ ___ _ _ _ __ _ _ ___| |_ ___ _ _
--    | | | ' \/ -_) | | ' \  _/ -_) '_| '_ \ '_/ -_)  _/ -_) '_|
--    |_| |_||_\___| |_|_||_\__\___|_| | .__/_| \___|\__\___|_|
--                                     |_|

-- | The parsed commands from the BF language.
--
-- Please have a look at this commands table:
--   https://en.wikipedia.org/wiki/Brainfuck#Commands
data BFCommand
  = -- | 'Loop x' is equivalent to `[x]` in BF. It is essentially a while loop.
    Loop [BFCommand]
  | -- | The `>` command.
    ShiftRight
  | -- | The `<` command.
    ShiftLeft
  | -- | The `+` command.
    Increment
  | -- | The `-` command.
    Decrement
  | -- | The `,` command.
    ReadInput
  | -- | The `.` command.
    WriteOutput
  deriving (Eq, Show)

type BFProgram = [BFCommand]

prependMaybe :: Maybe a -> [a] -> [a]
prependMaybe Nothing aa = aa
prependMaybe (Just x) xx = x : xx

-- | This is just a helper function.
parseProgram' :: String -> (BFProgram, String)
parseProgram' "" = ([], "")
parseProgram' ('[' : rest) = (Loop innerProgram : outerProgram, s')
  where
    (innerProgram, s) = parseProgram' rest
    (outerProgram, s') = parseProgram' s
parseProgram' (']' : rest) = ([], rest)
parseProgram' (x : rest) = (command `prependMaybe` restProgram, s)
  where
    (restProgram, s) = parseProgram' rest
    command = case x of
      '>' -> pure ShiftRight
      '<' -> pure ShiftLeft
      '+' -> pure Increment
      '-' -> pure Decrement
      '.' -> pure WriteOutput
      ',' -> pure ReadInput
      _ -> Nothing

-- | This function parses the given string and returns a sequence of BF
-- commands.
parseProgram :: String -> BFProgram
parseProgram = fst . parseProgram'

-- | Executes one BF command.
--
-- NOTE: You will probably need to call 'evaluateProgram' somewhere in
-- this function.
executeCommand :: BFCommand -> BFMonad ()
executeCommand ShiftLeft = shiftDataL
executeCommand ShiftRight = shiftDataR
executeCommand Increment = do
  cur <- readData
  writeData $ cur + 1
executeCommand Decrement = do
  cur <- readData
  writeData $ cur - 1
executeCommand ReadInput = readInput >>= writeData
executeCommand WriteOutput = readData >>= writeOutput
executeCommand (Loop commands) = do
  cur <- readData
  if cur == 0 then
    pure ()
  else do
    evaluateProgram commands
    executeCommand (Loop commands)

-- | This function should evaluate the whole program.
evaluateProgram :: BFProgram -> BFMonad ()
evaluateProgram commands = sequence_ $ executeCommand <$> commands

-- | This constant just contains an infinite empty tape. You can use this as
-- the initial data tape.
--
-- NOTE: It is infinite.
emptyTape :: Tape Int
emptyTape = Tape (repeat 0) 0 (repeat 0)

-- | In this function you should bring everything together and execute the given
-- list of commands.
--
-- The input stream is the second argument to the function.
--
-- The returned string should contain the output stream that the evaluation
-- produces.
--
-- NOTE: since the output stream of the program is written backwards, you
-- will need to reverse the output stream. (You can use the 'reverse' function.)
--
-- You will need to construct the initial state for the monad, evaluate the
-- program from the initial state, and convert the resulting value to the
-- appropriate type.
executeProgram ::
  -- | The text of the BF program.
  String ->
  -- | The input to pass into the program.
  String ->
  Maybe String
executeProgram pr_str input = let program = parseProgram pr_str
                                  mon = evaluateProgram program
                                  init_bfstate = BFState {bfDataTape=emptyTape, bfInput = ord <$> input, bfOutput = []}
                                  output_or_err = executeErrorState init_bfstate (mon *> get)
                                  in case output_or_err of 
                                    Left _ -> Nothing
                                    Right output -> Just $ reverse $ chr <$> (bfOutput output)


-- data BFState
--   = BFState
--       { -- | The data tape.
--         bfDataTape :: Tape Int,
--         -- | The input stream of the BF interpreter.
--         bfInput :: [Int],
--         -- | The output stream of the BF interpreter.
--         bfOutput :: [Int]
--       }
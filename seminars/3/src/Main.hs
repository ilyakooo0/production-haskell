{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell    #-}

module Main (main) where
import Data.Text.Lazy
import Data.Maybe
import Web.Scotty
import Task (Tape(..), shiftTapeR, shiftTapeL, emptyTape)
import qualified Control.Monad.State.Class  as ST
import Control.Monad.Reader.Class

import           Data.Acid
import           Data.SafeCopy
import           Data.Typeable
import Control.Monad.IO.Class


-- TASK:
-- 1 endpoint per BF command
-- read tape
-- read ouput
-- clear state



data BFState
  = BFState
      { -- | The data tape.
        bfDataTape :: Tape Int,
        -- | The output stream of the BF interpreter.
        bfOutput :: [Int]
      }
  deriving (Eq, Show)

data BFCommand
  = -- | The `>` command.
    ShiftRight
  | -- | The `<` command.
    ShiftLeft
  | -- | The `+` command.
    Increment
  | -- | The `-` command.
    Decrement
  | -- | The `,` command.
    ReadInput Int
  | -- | The `.` command.
    WriteOutput
  deriving (Eq, Show)

executeCommandOnTape :: BFState -> (Tape Int -> Maybe (Tape Int)) -> BFState
executeCommandOnTape state f =
      let tape = bfDataTape state
          new_tape = fromJust (f tape) 
      in state {bfDataTape = new_tape}

executeCommandAddToCell :: Int -> BFState  -> BFState
executeCommandAddToCell addValue state = 
  let tape = bfDataTape state
      old_value = tapeValue tape 
      new_tape = tape {tapeValue = old_value + addValue} in
   state {bfDataTape = new_tape}

executeCommand :: BFCommand -> BFState -> BFState
executeCommand ShiftLeft state =  
  executeCommandOnTape state shiftTapeL
executeCommand ShiftRight state =
  executeCommandOnTape state shiftTapeR

executeCommand Increment state = executeCommandAddToCell 1 state
executeCommand Decrement state = executeCommandAddToCell (-1) state

executeCommand (ReadInput new_value) state = 
      let tape = bfDataTape state
          new_tape = tape {tapeValue = new_value} in
   state {bfDataTape = new_tape}

executeCommand WriteOutput state = 
  let tape = bfDataTape state
      old_output = bfOutput state
      value = tapeValue tape 
  in state {bfOutput = value : old_output}

-------------- acid state boilerplateexecuteCommand

$(deriveSafeCopy 0 'base ''Tape)
$(deriveSafeCopy 0 'base ''BFState)
$(deriveSafeCopy 0 'base ''BFCommand)

-------------  add transactions
updateStateByExecutingCommand :: BFCommand -> Update BFState ()
updateStateByExecutingCommand command = do
  currentState <- ST.get
  -- let newState = 
  ST.put $ executeCommand command currentState
      
resetState :: Update BFState ()
resetState = do
  ST.put BFState {
    bfDataTape = emptyTape,
    bfOutput = []
  }

queryTape :: Query BFState (Tape Int)
queryTape = bfDataTape <$> ask

queryOutput :: Query BFState [Int]
queryOutput = bfOutput <$> ask

--queryState :: Query HelloWorldState String
--queryState = do HelloWorldState string <- ask
--                return string

$(makeAcidic ''BFState ['updateStateByExecutingCommand, 'resetState, 'queryTape, 'queryOutput])


------------------------------

main :: IO ()
main = do
  acidstate <- openLocalState (BFState {bfDataTape = emptyTape, bfOutput = []})
  scotty 3000 $ do
    -- /add/8/12 -> 20
    get "/tape" $ do
      tape <- liftIO $ query acidstate QueryTape
      text $ pack . show $ tape
    post "/increment" $ do 
      liftIO $ update acidstate (UpdateStateByExecutingCommand Increment)
    get "/add/:first/:second" $ do
      first <- captureParam "first"
      second <- captureParam "second"
      let result:: Integer = first + second
      text $ pack . show $ result
    get "/:word" $ do
      beam <- captureParam "word"
      text beam
    
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( server
    ) where

import Web.Scotty
import Data.IORef 
import qualified Data.Text.Lazy
import Control.Monad.Trans

server :: IO ()
server = do
  result <- newIORef 0
  scotty 3000 $ do
    get "/:word" $ do
      beam <- pathParam "word"
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]
    post "/add/:number" $ do
      number :: Int <- pathParam "number"
      result <- lift $ atomicModifyIORef result (\x -> (x + number, x + number))
        -- modifyIORef :: IORef a -> (a -> a) -> IO () 
        -- atomicModifyIORef :: IORef a -> (a -> (a, b)) -> IO b
      text $ Data.Text.Lazy.pack $ show result
      
-- pure :: a -> ActionT IO a
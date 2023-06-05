module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)

data AppState = AppState
  { counter :: IORef Int
  }

type App = ReaderT AppState IO

-- Helper function to increment the counter
incrementCounter :: App ()
incrementCounter = do
  state <- ask
  liftIO $ modifyIORef (counter state) (+ 1)

-- Example render function
render :: App ()
render = do
  state <- ask
  count <- liftIO $ readIORef (counter state)
  liftIO $ putStrLn $ "Counter: " ++ show count

-- Example state transformation function:q
stateTransformation :: App ()
stateTransformation = do
  incrementCounter
  incrementCounter

-- Example main function
main :: IO ()
main = do
  counterRef <- newIORef 0
  let initialState = AppState { counter = counterRef }
  runReaderT (render >> stateTransformation >> render) initialState

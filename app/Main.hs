module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Graphics.Gloss.Interface.IO.Game -- (Picture, playIO)

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
render :: App Picture
render = do
  state <- ask
  count <- liftIO $ readIORef (counter state)
  liftIO $ putStrLn $ "Counter: " ++ show count
  -- Return a placeholder picture for demonstration
  pure $ error "Placeholder picture"

-- Example state transformation function
stateTransformation :: Float -> App AppState
stateTransformation _dt = do
  incrementCounter
  incrementCounter

-- Example main function
main :: IO ()
main = do
  counterRef <- newIORef 0
  let initialState = AppState { counter = counterRef }
  playIO
    (InWindow "My Game" (800, 600) (200, 200)) -- Display configuration
    white                                      -- Background color
    60                                         -- Frames per second
    initialState                               -- Initial state
    (runReaderT render)                        -- Render function
    (\dt state -> runReaderT (stateTransformation dt) state)
 -- State transformation function

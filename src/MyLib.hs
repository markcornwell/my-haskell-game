module MyLib where

import Graphics.Gloss.Interface.IO.Game
import Control.Monad.Reader
import Data.IORef

data GameMode = SquareMode | CircleMode

data GameState = GameState
    { circlePos :: IORef Point
    , circleVel :: IORef Vector
    , gameMode  :: IORef GameMode
    }

type AppM = ReaderT GameState IO

handleEvent :: Event -> AppM ()
handleEvent (EventKey (Char 's') Down _ _) = do
    gm <- liftIO $ readIORef =<< asks gameMode
    case gm of
        SquareMode -> liftIO $ modifyIORef' =<< asks gameMode $ const CircleMode
        CircleMode -> liftIO $ modifyIORef' =<< asks gameMode $ const SquareMode
handleEvent _ = return ()

updateGame :: Float -> AppM ()
updateGame deltaTime = do
    cv <- liftIO $ readIORef =<< asks circleVel
    cp <- liftIO $ readIORef =<< asks circlePos
    liftIO $ modifyIORef' (circlePos =<< asks circlePos) (\(x, y) -> moveCircle (cv x y) deltaTime (x, y))

drawGame :: AppM Picture
drawGame = do
    gm <- liftIO $ readIORef =<< asks gameMode
    cp <- liftIO $ readIORef =<< asks circlePos
    case gm of
        SquareMode -> return (translateX =<< liftIO (Data.IORef.readIORef cp))
        CircleMode -> return (renderCircle =<< liftIO (Data.IORef.readIORef cp))

moveCircle :: Vector -> Float -> Point -> Point
moveCircle (vx, vy) dt (x, y) = (x + vx * dt, y + vy * dt)

renderCircle :: Point -> Picture
renderCircle (x, y) = translate x y $ color red $ circleSolid 50

initialState :: IO GameState
initialState = do
    circlePosRef <- newIORef (0, 0)
    circleVelRef <- newIORef (100, 100)
    gameModeRef <- newIORef SquareMode
    return $ GameState circlePosRef circleVelRef gameModeRef

{-
runGame :: IO ()
runGame = do
    initialState' <- initialState
    runAppM initialState' $ playIO
        (InWindow "My Haskell Game" (windowWidth, windowHeight) (100, 100))
        white 60 drawGame handleEvent updateGame

-}

translateX :: Point -> IO Picture
translateX (x, y) = do
  let squareSize = 50
      picture = rectangleSolid squareSize squareSize
  return (translate x y picture)


runGame :: IO ()
runGame = do
    initialState' <- initialState
    renderFunc <- renderFunction
    let appM = runApp initialState'
    playIO
        (InWindow "My Game" (800, 600) (200, 200))
        white
        60
        initialState'
        (\state -> runReaderT (renderFunc state) appM)
        (\event state -> runReaderT (handleEvent event) appM >>= \s -> return (s, s))
        (\dt state -> runReaderT (updateGame dt) appM >>= \s -> return (s, s))

module Lib where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit (exitSuccess)

data GameMode = SquareMode | CircleMode deriving Eq

data GameState = GameState
    { circlePos :: Point      -- Position of the circle
    , circleVel :: Vector     -- Velocity of the circle
    , gameMode :: GameMode    -- Current game mode (SquareMode or CircleMode)
    }

windowWidth :: Int
windowWidth = 800

windowHeight :: Int
windowHeight = 600

circleRadius :: Float
circleRadius = 50

someFunction :: IO ()
someFunction = playIO (InWindow "My Haskell Game" (windowWidth, windowHeight) (100, 100)) -- Window settings
                      white                    -- Background color
                      60                       -- Frames per second
                      initialState             -- Initial game state
                      drawGame                 -- Function to draw the game
                      handleEvent              -- Function to handle events
                      updateGame               -- Function to update the game state

initialState :: GameState
initialState = GameState
    { circlePos = (0, 0)
    , circleVel = (100, 100)
    , gameMode = SquareMode  -- Start with SquareMode
    }

drawGame :: GameState -> IO Picture
drawGame gameState
  | gameMode gameState == SquareMode = return (translateX (circlePos gameState)) -- Draw square in SquareMode
  | gameMode gameState == CircleMode = return (renderCircle (circlePos gameState)) -- Draw circle in CircleMode


handleEvent :: Event -> GameState -> IO GameState
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
handleEvent (EventKey (Char 'c') Down _ _) gameState = return gameState { gameMode = CircleMode } -- Switch to CircleMode
handleEvent (EventKey (Char 's') Down _ _) gameState = return gameState { gameMode = SquareMode } -- Switch to SquareMode
handleEvent _ gameState = return gameState

updateGame :: Float -> GameState -> IO GameState
updateGame deltaTime gameState = do
    let circlePos'@(x, y) = moveCircle (circleVel gameState) deltaTime (circlePos gameState)
        (minX, minY) = (-fromIntegral windowWidth / 2 + circleRadius, -fromIntegral windowHeight / 2 + circleRadius)
        (maxX, maxY) = ( fromIntegral windowWidth / 2 - circleRadius,  fromIntegral windowHeight / 2 - circleRadius)
        circleVel'@(vx, vy) = if x < minX || x > maxX then (-fst (circleVel gameState), snd (circleVel gameState)) else circleVel gameState
        circleVel'' = if y < minY || y > maxY then (fst circleVel', -snd circleVel') else circleVel'
    return gameState { circlePos = clampPoint circlePos' (minX, minY) (maxX, maxY), circleVel = circleVel'' }

moveCircle :: Vector -> Float -> Point -> Point
moveCircle (vx, vy) dt (x, y) = (x + vx * dt, y + vy * dt)

clampPoint :: Point -> Point -> Point -> Point
clampPoint (x, y) (minX, minY) (maxX, maxY) =
    (clamp x minX maxX, clamp y minY maxY)

clamp :: (Ord a) => a -> a -> a -> a
clamp val lowerBound upperBound
    | val < lowerBound = lowerBound
    | val > upperBound = upperBound
    | otherwise = val

translateX :: Point -> Picture
translateX (x, y) = translate x y (color red (rectangleSolid 100 100))

render :: GameState -> IO Picture
render gameState
    | gameMode gameState == SquareMode = return (translateX (circlePos gameState)) -- Draw square in SquareMode
    | gameMode gameState == CircleMode = return (renderCircle (circlePos gameState)) -- Draw circle in CircleMode

renderCircle :: Point -> Picture
renderCircle (x, y) = translate x y $ color red $ circleSolid circleRadius

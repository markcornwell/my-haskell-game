module Lib where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Exit (exitSuccess)

data GameState = GameState
    { circlePos :: Point      -- Position of the circle
    , circleVel :: Vector     -- Velocity of the circle
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
    }

drawGame :: GameState -> IO Picture
drawGame gameState = do
    return (translateX (circlePos gameState))

handleEvent :: Event -> GameState -> IO GameState
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
handleEvent _ gameState = return gameState

updateGame :: Float -> GameState -> IO GameState
updateGame deltaTime gameState = do
    let circlePos'@(x, y) = moveCircle (circleVel gameState) deltaTime (circlePos gameState)
        (minX, minY) = (-fromIntegral windowWidth / 2 + circleRadius, -fromIntegral windowHeight / 2 + circleRadius)
        (maxX, maxY) = (fromIntegral windowWidth / 2 - circleRadius, fromIntegral windowHeight / 2 - circleRadius)
        circleVel' = if x < minX || x > maxX then (-fst (circleVel gameState), snd (circleVel gameState)) else (fst (circleVel gameState), snd (circleVel gameState))
    return gameState { circlePos = clampPoint circlePos' (minX, minY) (maxX, maxY), circleVel = circleVel' }

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
translateX (x, y) = translate x y (color red (rectangleSolid 100 50))

module MyLib 
    ( someFunction
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy
import System.Exit (exitSuccess)
-- import qualified Data.ByteString as ByteString

{- image stuff starts here -}
import Codec.BMP
import Graphics.Gloss.Data.Bitmap (BitmapData(..))



{- end of image stuff -}

data GameMode = SquareMode | CircleMode | TextMode deriving Eq

data GameState = GameState
    { circlePos :: Point      -- Position of the circle
    , circleVel :: Vector     -- Velocity of the circle
    , gameMode :: GameMode    -- Current game mode (SquareMode or CircleMode)
    , font :: Maybe Picture   -- Holds a picture of the fonts
    }

windowWidth :: Int
windowWidth = 800

windowHeight :: Int
windowHeight = 600

circleRadius :: Float
circleRadius = 50

fontPath :: String
fontPath = "assets/myFont.bmp"

someFunction :: IO ()
someFunction = do
    -- myfont <- loadJuicyPNG fontPath
    myBMP <- safeLoadBMP fontPath
    case myBMP of
        Nothing -> error "Missing or corrupted font file"
        Just bmp -> do
            let myData = bitmapDataOfBMP bmp
            let myRect = Rectangle { rectPos = (0,0), rectSize = (500,520)}
            let myPict = BitmapSection myRect myData
            let state = initialState { font = Just myPict }
            playIO
                (InWindow "My Haskell Game" (windowWidth, windowHeight) (100, 100)) -- Window settings
                white                    -- Background color
                60                       -- Frames per second
                state                    -- Initial game state
                drawGame                 -- Function to draw the game
                handleEvent              -- Function to handle events
                updateGame               -- Function to update the game state

safeLoadBMP :: String -> IO (Maybe BMP)
safeLoadBMP path = do
  either <- readBMP path
  case either of 
    Left msg -> pure Nothing
    Right bmp -> pure $ Just bmp

initialState :: GameState
initialState = GameState
    { circlePos = (0, 0)
    , circleVel = (100, 100)
    , gameMode = TextMode 
    , font = Nothing
    }

{-
drawGame :: GameState -> IO Picture
drawGame gameState
    | gameMode gameState == SquareMode = return (translateX (circlePos gameState)) -- Draw square in SquareMode
    | gameMode gameState == CircleMode = return (renderCircle (circlePos gameState)) -- Draw circle in CircleMode
-}

drawGame :: GameState -> IO Picture
drawGame gameState = do 
    case gameMode gameState of 
        SquareMode -> return (translateX (circlePos gameState)) -- Draw square in SquareMode
        CircleMode -> return (renderCircle (circlePos gameState)) -- Draw circle in CircleMode
        TextMode -> case font gameState of 
                        Nothing -> error "the impossible happended"
                        Just image -> return image


handleEvent :: Event -> GameState -> IO GameState
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
handleEvent (EventKey (Char 'c') Down _ _) gameState = return gameState { gameMode = CircleMode } -- Switch to CircleMode
handleEvent (EventKey (Char 's') Down _ _) gameState = return gameState { gameMode = SquareMode } -- Switch to SquareMode
handleEvent (EventKey (Char 't') Down _ _) gameState = return gameState { gameMode = TextMode } -- Switch to SquareMode
handleEvent _ gameState = return gameState

updateGame :: Float -> GameState -> IO GameState
updateGame deltaTime gameState = do
    let circlePos'@(x, y) = moveCircle (circleVel gameState) deltaTime (circlePos gameState)
        (minX, minY) = (-fromIntegral windowWidth / 2 + circleRadius, -fromIntegral windowHeight / 2 + circleRadius)
        (maxX, maxY) = ( fromIntegral windowWidth / 2 - circleRadius,  fromIntegral windowHeight / 2 - circleRadius)
        circleVel'@(_vx, _vy) = if x < minX || x > maxX then (-fst (circleVel gameState), snd (circleVel gameState)) else circleVel gameState
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

renderCircle :: Point -> Picture
renderCircle (x, y) = translate x y $ color red $ circleSolid circleRadius







module MyLib 
    ( someFunction
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy
import System.Exit (exitSuccess)

import Codec.BMP
import Graphics.Gloss.Data.Bitmap (BitmapData(..))

data GameMode = SquareMode | CircleMode | TextMode | FontMode deriving Eq

data GameState = GameState
    { circlePos :: Point      -- Position of the circle
    , circleVel :: Vector     -- Velocity of the circle
    , gameMode :: GameMode    -- Current game mode (SquareMode or CircleMode)
    , fontPic :: Maybe Picture   -- Holds a picture of the fonts
    , font :: Font
    , flyingChar :: Int
    }

data Font = Font
    { fontData :: BitmapData
    , fontOriginX :: Int
    , fontOriginY :: Int
    , fontRows :: Int  -- starting at row 0 .. fontRows -1
    , fontCols :: Int  -- starting at col 0
    , fixed :: Bool  -- true if fixed width font
    , fontCharHeight :: Int  -- character height in fontData
    , fontCharWidth  :: Int  -- character width in fontData
    }

fontCharPicture :: Font -> Int -> Picture
fontCharPicture font i = 
    bitmapSection (fontCharRect font i) (fontData font)  -- is this picture centered at (0,0) ?



-- | computes a rectangle around the ith character in the font 
fontCharRect :: Font -> Int -> Rectangle
fontCharRect font i =
    let x0 = fontOriginX font
        y0 = fontOriginY font
        h  = fontCharHeight font
        w  = fontCharWidth font
        col = fontCol font i -- x position        
        row = fontRow font i -- y position
    in
        Rectangle { rectPos  = (x0 + w * col, y0 + h * row)
                  , rectSize = (w,h)
                  }

fontCol :: Font -> Int -> Int
fontCol f i = i `mod` fontCols f

fontRow :: Font -> Int -> Int
fontRow f i = fontRows f - i `div` fontCols f - 1


initFont :: String -> IO (Maybe Font)
initFont path = do
    maybeBMP <- safeLoadBMP fontPath
    case maybeBMP of 
        Just bmp -> pure $
            Just Font { fontData = bitmapDataOfBMP bmp
                      , fontOriginX = 15
                      , fontOriginY = 15
                      , fontRows = 6
                      , fontCols = 11
                      , fixed = True
                      , fontCharHeight = 85
                      , fontCharWidth  = 40
                      }
        Nothing -> pure Nothing 

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
    maybeFont <- initFont fontPath
    -- myBMP <- safeLoadBMP fontPath
    -- case myBMP of
    case maybeFont of
        Nothing -> error "Missing or corrupted font file"
        -- Just bmp -> do
        Just theFont -> do
            -- let myData = bitmapDataOfBMP bmp
            let myData = fontData theFont
            let myRect = Rectangle { rectPos = (0,0), rectSize = (500,520)}
            let myPict = BitmapSection myRect myData
            let state = initialState { fontPic = Just myPict , font = theFont }
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
    , circleVel = (0,0) -- was (100, 100)
    , gameMode = TextMode 
    , fontPic = Nothing
    , flyingChar = 0
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
        TextMode -> case fontPic gameState of 
                        Nothing -> error "the impossible happended"
                        Just image -> return image
                        -- Just image -> return $ pictures [ image, renderFontCharGrid (font gameState) ]
        FontMode -> return (renderFontChar (font gameState) (flyingChar gameState) (circlePos gameState))
--        FontMode -> return (renderChar circlePos)


handleEvent :: Event -> GameState -> IO GameState
handleEvent (EventKey (SpecialKey KeyEsc) Down _ _) _ = exitSuccess
handleEvent (EventKey (Char 'c') Down _ _) gameState = return gameState { gameMode = CircleMode } 
handleEvent (EventKey (Char 's') Down _ _) gameState = return gameState { gameMode = SquareMode } 
handleEvent (EventKey (Char 't') Down _ _) gameState = return gameState { gameMode = TextMode }
handleEvent (EventKey (Char 'f') Down _ _) gameState = return gameState { gameMode = FontMode }
handleEvent (EventKey (Char '0') Down _ _) gameState = return gameState { gameMode = FontMode , flyingChar = 0}
handleEvent (EventKey (Char '1') Down _ _) gameState = return gameState { gameMode = FontMode , flyingChar = 1}
handleEvent (EventKey (Char '+') Down _ _) gameState = return gameState { gameMode = FontMode , flyingChar = flyingChar gameState + 1}
handleEvent (EventKey (Char '-') Down _ _) gameState = return gameState { gameMode = FontMode , flyingChar = flyingChar gameState - 1}
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

renderFontChar :: Font -> Int -> Point -> Picture 
renderFontChar f i (x,y) = 
    pictures 
        [ translate x y $ fontCharPicture f i 
        , translate x y $ rectangleWire (fromIntegral (fontCharWidth f ))
                                        (fromIntegral (fontCharHeight f ))
        , translate (x + 50) (y + 50) 
            $ scale 0.25 0.25 
            $ text $ show i ++ "  c" ++ show (fontCol f i )++ "r" ++ show (fontRow f i)
        ]

renderFontCharBox :: Font -> Int -> Picture
renderFontCharBox font i = 
    let (x,y) = rectPos  $ fontCharRect font i
        (h,w) = rectSize $ fontCharRect font i
    in 
       translate (fromIntegral x) (fromIntegral y) 
       $ rectangleWire (fromIntegral (fontCharWidth font ))
                                     (fromIntegral (fontCharHeight font ))

renderFontCharGrid :: Font -> Picture
renderFontCharGrid f =
    let n = fontRows f * fontCols f - 1
    in
        pictures $ map (renderFontCharBox f) [0..n]
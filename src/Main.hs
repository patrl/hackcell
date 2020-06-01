-- a toy brogue console
--

{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import SDL (($=))
import qualified SDL -- the main sdl2 bindings
-- TODO figure out if juicypixels can replace this, although it's probably slower.
import qualified SDL.Image                     as SDLImg -- necessary to load the brogue font
-- import qualified SDL.Font                      as TTF
import           SDL.Vect -- We need this for the vector constructors
import           Control.Monad                  ( unless ) -- a handy utility for breaking a loop
import           Foreign.C.Types                ( CInt,CFloat ) -- since we dealing with a C FFI library, we need to deal with C Integer types
import           Data.Word                      ( Word8 ) -- RGBA colors are stored in this format
import           Lens.Micro                     ( (^.) )
import qualified Data.Vector                   as V
import           Control.Concurrent             ( threadDelay )
import qualified Math.Geometry.Grid            as Grid
import           Math.Geometry.Grid.Square

fontName :: FilePath
fontName = "font-13.png"

-- directory where resources reside.
resourceDir :: FilePath
resourceDir = "data/"

-- screen height and width in tiles.
screenDims :: V2 CInt
screenDims = V2 80 60

-- screenGrid
screenGrid :: RectSquareGrid
screenGrid = rectSquareGrid (fromIntegral $ screenDims ^. _y)
                            (fromIntegral $ screenDims ^. _x)

-- screen grid index to rect
screenGridIndexToRect :: (Int, Int) -> V2 CInt -> SDL.Rectangle CInt
screenGridIndexToRect (xCoord, yCoord) tileDims =
  mkRect (V2 (fromIntegral xCoord) (fromIntegral yCoord) * tileDims) tileDims

scalingFactor :: V2 CFloat
scalingFactor = V2 1.0 1.0

_font13grid :: V2 CInt
_font13grid = V2 16 16

-- TODO find out if there's a nice library for interfacing with RGBA data
-- some handy color aliases
black, white, _clearColor, dracBlack, dracRed, dracGreen :: V4 Word8
black = V4 0 0 0 0
white = V4 maxBound maxBound maxBound 0
_clearColor = dracBlack

dracBlack = V4 40 42 54 0
dracRed = V4 maxBound 85 85 0
dracGreen = V4 80 250 123 0
dracYello = V4 241 250 140 0


-- takes a texture atlas, together with the rows/columns, and returns an SDL surface
-- together with the dimensions of each tile.
loadSurfaceWithTileDims
  :: FilePath -> V2 CInt -> IO (SDL.Surface, V2 CInt, V2 CInt)
loadSurfaceWithTileDims path gridDims = do
  surface     <- SDLImg.load $ resourceDir ++ path
  surfaceDims <- SDL.surfaceDimensions surface
  return
    ( surface
    , surfaceDims
    , V2 (surfaceDims ^. _x `div` gridDims ^. _x)
         (surfaceDims ^. _y `div` gridDims ^. _y)
    )

-- takes a surface, and loads it as a texture, discarding the surface from memory.
loadTextureFromSurface :: SDL.Surface -> SDL.Renderer -> IO SDL.Texture
loadTextureFromSurface surface renderer = do
  newTexture <- SDL.createTextureFromSurface renderer surface
  SDL.freeSurface surface
  return newTexture

-- a helper function from two 2-dimensional vectors to an SDL rectangle.
mkRect :: V2 CInt -> V2 CInt -> SDL.Rectangle CInt
mkRect p = SDL.Rectangle (P p)

-- takes some texture dimensions, and grid dimensions, and returns.
mkRects :: V2 CInt -> V2 CInt -> IO (V.Vector (SDL.Rectangle CInt))
mkRects textDims gDims = do
  let cDims =
        V2 (textDims ^. _x `div` gDims ^. _x) (textDims ^. _y `div` gDims ^. _y)
  return $ V.fromList
    [ mkRect (V2 px py) cDims
    | px <- [0, cDims ^. _x .. (textDims ^. _x - cDims ^. _x)]
    , py <- [0, cDims ^. _y .. (textDims ^. _y - cDims ^. _y)]
    ]

main :: IO ()
main = do
  SDL.initialize ([SDL.InitVideo] :: [SDL.InitFlag])

  -- load font, and get the dimensions of each tile
  (atlasSurface, surfaceDims, tileDims) <- loadSurfaceWithTileDims
    fontName
    _font13grid
  putStrLn $ "the dimensions of each tile are:" ++ show tileDims -- for debugging purposes

  -- compute the rectangles corresponding to each tile
  rects  <- mkRects surfaceDims _font13grid

  window <- SDL.createWindow
    "SDL Tutorial"
    SDL.defaultWindow { SDL.windowInitialSize = screenDims * tileDims
                      , SDL.windowHighDPI     = True
                      , SDL.windowMode        = SDL.Fullscreen
                      } -- load a window -- the dimension is a multiple of tile size

  renderer         <- SDL.createRenderer window (-1) SDL.defaultRenderer -- creates the rendering context

  _ <- SDL.rendererScale renderer $= scalingFactor

  _ <- SDL.rendererDrawColor renderer $= _clearColor

  brogueTexture    <- loadTextureFromSurface atlasSurface renderer

  let broguePrintChar charNum gridCoords = SDL.copy
        renderer
        brogueTexture
        (Just $ rects V.! charNum)
        (Just $ screenGridIndexToRect gridCoords (tileDims))

  let
    loop = do
      events <- map SDL.eventPayload <$> SDL.pollEvents
      let quit = SDL.QuitEvent `elem` events
      _ <- SDL.rendererDrawColor renderer $= _clearColor
      SDL.clear renderer
      broguePrintChar 16 (40,30)
      -- _ <- SDL.rendererDrawColor renderer $= dracRed
      -- _ <- SDL.fillRect renderer (Just $ screenGridIndexToRect (0,0) (tileDims * 8))
      sequence_ [ broguePrintChar 16 coords | coords <- Grid.centre screenGrid ]
      SDL.present renderer
      unless quit loop
  loop

  SDL.destroyTexture brogueTexture
  SDL.destroyRenderer renderer
  SDL.destroyWindow window

  -- TTF.free inputFont
  -- TTF.quit
  SDL.quit

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Foreign.C.Types                ( CInt )
import qualified SDL
import           SDL.Vect
import           Control.Concurrent             ( threadDelay )

screenWidth :: CInt
screenWidth = 800
screenHeight :: CInt
screenHeight = 600

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]

  window <- SDL.createWindow
    "SDL Tutorial"
    SDL.defaultWindow { SDL.windowInitialSize = V2 screenWidth screenHeight }

  screenSurface <- SDL.getWindowSurface window
  let white = V4 maxBound maxBound maxBound maxBound
  SDL.surfaceFillRect screenSurface Nothing white
  SDL.updateWindowSurface window

  threadDelay 2000000

  SDL.destroyWindow window
  SDL.quit

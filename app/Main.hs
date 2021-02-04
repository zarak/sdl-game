{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified MyLib (someFunc)
import Linear (V4(..))
import Control.Monad (unless)
import SDL

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  appLoop renderer
  destroyWindow window

appLoop :: Renderer -> IO ()
appLoop renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events

  -- Background color
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer

  -- Rectangle color
  rendererDrawColor renderer $= V4 255 255 255 255
  let rect = Rectangle (P $ V2 30 40) (V2 100 200)
  drawRect renderer $ Just rect

  present renderer

  unless qPressed (appLoop renderer)

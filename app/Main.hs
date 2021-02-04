{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified MyLib (someFunc)
import Linear (V4(..))
import Control.Monad (unless)
import Data.IORef
import SDL

import GameState

main :: IO ()
main = do
  initializeAll
  window <- createWindow "My SDL Application" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer

  playerRef <- newIORef (Player 300 300, neutralDirectionKeyState)

  appLoop renderer playerRef
  destroyWindow window

appLoop :: Renderer -> IORef (Player, DirectionKeyState) -> IO ()
appLoop renderer playerRef = do
  events <- pollEvents

  (player, dirKeySt) <- readIORef playerRef

  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
              keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events

      dirKeySt' = foldr updateDirection dirKeySt events
      player' = integratePlayer dirKeySt' player

  putStrLn "Player: "
  print player'
  
  writeIORef playerRef (player', dirKeySt')

  -- Background color
  rendererDrawColor renderer $= V4 0 0 0 255
  clear renderer

  -- Rectangle color
  rendererDrawColor renderer $= V4 255 255 255 255
  let rect = Rectangle (P $ playerPos player') (V2 10 10)
  drawRect renderer $ Just rect

  present renderer

  unless qPressed (appLoop renderer playerRef)

updateDirection :: Event -> DirectionKeyState -> DirectionKeyState
updateDirection event dirKeySt =
  case eventPayload event of 
    KeyboardEvent keyboardEvent ->
      let isPressed = keyboardEventKeyMotion keyboardEvent == Pressed 
          keyCode = keysymKeycode (keyboardEventKeysym keyboardEvent) 
       in case keyCode of 
            KeycodeW -> dirKeySt { directionKeyPressedUp = isPressed } 
            KeycodeA -> dirKeySt { directionKeyPressedLeft = isPressed } 
            KeycodeS -> dirKeySt { directionKeyPressedDown = isPressed } 
            KeycodeD -> dirKeySt { directionKeyPressedRight = isPressed } 
            _ -> dirKeySt 
    _ -> dirKeySt

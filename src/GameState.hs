module GameState
  ( Player (..)

  , DirectionKeyState (..)
  , neutralDirectionKeyState

  , integratePlayer
  ) where

import Foreign.C.Types (CInt (..))
import Linear
import qualified SDL as S

data Player = Player
  { playerPos :: !(V2 CInt)
  , playerVel :: !(V2 CInt)
  } deriving Show

data DirectionKeyState = DirectionKeyState
  { directionKeyPressedUp    :: !Bool
  , directionKeyPressedDown  :: !Bool
  , directionKeyPressedLeft  :: !Bool
  , directionKeyPressedRight :: !Bool
  }

neutralDirectionKeyState :: DirectionKeyState
neutralDirectionKeyState = DirectionKeyState
  { directionKeyPressedUp    = False
  , directionKeyPressedDown  = False
  , directionKeyPressedLeft  = False
  , directionKeyPressedRight = False
  }

integratePlayer :: DirectionKeyState -> Player -> Player
integratePlayer dirKeySt player =
  let vel' = commandVelocity dirKeySt
  in  Player { playerPos = playerPos player + 10
             , playerVel = vel' }

commandVelocity :: DirectionKeyState -> V2 CInt
commandVelocity dirKeySt = velUp + velDown + velLeft + velRight
  where
    velUp    = if directionKeyPressedUp    dirKeySt then V2 0  (-1) else V2 0 0
    velDown  = if directionKeyPressedDown  dirKeySt then V2 0    1  else V2 0 0
    velLeft  = if directionKeyPressedLeft  dirKeySt then V2 (-1) 0  else V2 0 0
    velRight = if directionKeyPressedRight dirKeySt then V2   1  0  else V2 0 0

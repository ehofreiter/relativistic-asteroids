module Key where

import qualified SDL

data Key = KeyChar Char
         | KeyLeft
         | KeyRight
         | KeyUp
         | KeyDown
         | KeySpace
         | KeyEscape
         | KeyOther
         deriving (Eq, Ord, Show)

keyFromSDLKey :: SDL.Keysym -> Key
keyFromSDLKey sdlK = case SDL.keysymScancode sdlK of
  SDL.ScancodeLeft -> KeyLeft
  SDL.ScancodeRight -> KeyRight
  SDL.ScancodeUp -> KeyUp
  SDL.ScancodeDown -> KeyDown
  SDL.ScancodeSpace -> KeySpace
  SDL.ScancodeEscape -> KeyEscape
  otherwise -> KeyOther

{-# LANGUAGE OverloadedStrings #-}
module Run where

import Prelude hiding ((.), id)

import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Control.Concurrent (threadDelay)

import Data.IORef
import Data.Text (pack)
import Data.Set (Set)
import qualified Data.Set as Set

import Data.Time.Clock
--import System.Random
import Linear (V2(..), V4(..))

import qualified SDL
import SDL (($=))
import qualified SDL.Raw

import qualified Graphics.Rendering.OpenGL.GL as GL
import qualified Graphics.Rendering.OpenGL.GLU as GLU
import qualified Graphics.GLUtil as GLUtil

import Control.Wire hiding (unless)

import Key

run :: (MonadIO m)
       => m context -- Action to create window system context (e.g. window handle)
       -> (context -> m ()) -- Action to cleanup context after the program exits
       -> (context -> m input) -- IO action to get input (sense)
       -> (context -> output -> m ()) -- IO action to provide output (actuate)
       -> Wire (Timed NominalDiffTime ()) e m input output -- Wire to be run
       -> m ()
run init finish sense actuate wire = do
  context <- init
  mainLoop context sense actuate wire
  finish context

framerate = 60 -- frames per second
timeStep = 1/framerate -- seconds

mainLoop :: (MonadIO m)
            => context
            -> (context -> m input)
            -> (context -> output -> m ())
            -> Wire (Timed NominalDiffTime ()) e m input output
            -> m ()
mainLoop context sense actuate wire = do
  t0 <- liftIO getCurrentTime
  loop t0 wire
  -- t0 is the time we have finished simulating up to so far.
  where loop t0 wire = do
          t1 <- liftIO getCurrentTime
          let dt = diffUTCTime t1 t0
          -- If less than the time step has elapsed, release the CPU
          -- to other resources.
          if dt < timeStep
--            then do liftIO $ threadDelay $ max 0 $ round $ (timeStep - dt) * 10^6 - 100
--                    loop t0 wire
            then loop t0 wire
            else do input <- Right <$> sense context
                    -- Step the main wire using the given timeStep,
                    -- meaning we have now simulated up to time (t0 +
                    -- timeStep).
                    (output, wire') <- stepWire wire (Timed timeStep ()) input
                    case output of
                      Left _ -> return () -- Exit loop
                      Right o -> do actuate context o
                                    -- If we are more than 10 frames
                                    -- behind, set the simulation time
                                    -- to now.
                                    let t0' = if dt > 10 * timeStep
                                              then t1
                                              else addUTCTime timeStep t0
                                    loop t0' wire'

initSDL :: (MonadIO m, Integral a)
        => String -> a -> a -> m (SDL.Renderer, IORef (Set Key))
initSDL title width height = do
  SDL.initializeAll
  window <- SDL.createWindow (pack title) initialWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  keysRef <- liftIO $ newIORef (Set.empty)
  return (renderer, keysRef)
    where initialWindow =
            SDL.defaultWindow { SDL.windowOpenGL = Nothing --Just SDL.defaultOpenGL
                              , SDL.windowInitialSize = size }
          size = V2 (fromIntegral width) (fromIntegral height)

senseSDL :: (MonadIO m) => (a, IORef (Set Key)) -> m (Set Key)
senseSDL (_, keysRef) = do
  sdlEvents <- SDL.pollEvents
  liftIO $ modifyIORef keysRef (\keys -> foldl (flip keysDownSDL) keys sdlEvents)
  liftIO $ readIORef keysRef

-- TODO: Use mkPure to create the (Set Key) of pressed keys rather
-- than using an IORef.
{-
keysDown :: (Monad m) => Wire s e (StateT (Set Key) m) [SDL.Event] (Set Key)
keysDown = mkGenN f
  where f sdlEvents = do
          modify' (\keys -> foldl (flip keysDownSDL) keys sdlEvents)
          keys <- get
          return (Right keys, keysDown)
-}
keysDownSDL :: SDL.Event -> Set Key -> Set Key
keysDownSDL (SDL.Event { SDL.eventPayload = SDL.KeyboardEvent
                            SDL.KeyboardEventData { SDL.keyboardEventKeyMotion = km
                                                  , SDL.keyboardEventKeysym = ks
                                                  , SDL.keyboardEventRepeat = False } }) =
  case km of
    SDL.Pressed -> Set.insert (keyFromSDLKey ks)
    SDL.Released -> Set.delete (keyFromSDLKey ks)
keysDownSDL otherwise = id

-- An action that renders something to the given SDL renderer.
type SDLRender m = SDL.Renderer -> m ()

actuateSDL :: (MonadIO m) => (SDL.Renderer, a) -> SDLRender m -> m ()
actuateSDL (renderer, _) drawAction = do
  -- Clear back buffer to black.
  SDL.rendererDrawColor renderer $= V4 0 0 0 255
  SDL.clear renderer
  -- Perform the drawing action.
  drawAction renderer
  -- Swap buffers.
  SDL.present renderer

data GLResources = GLResources { vertBuffObj :: GL.BufferObject
                               , program :: GL.Program
                               , sdlglContext :: SDL.GLContext
                               , sdlWindow :: SDL.Window
                               , keysRef :: IORef (Set Key)
                               }

initSDL_GL :: (MonadIO m, Integral a)
           => String -> a -> a -> m GLResources
initSDL_GL title width height = do
  SDL.initializeAll
  window <- SDL.createWindow (pack title) initialWindow
  context <- SDL.glCreateContext window
  
  SDL.Raw.glSetAttribute SDL.Raw.SDL_GL_MULTISAMPLEBUFFERS 1
  SDL.Raw.glSetAttribute SDL.Raw.SDL_GL_MULTISAMPLESAMPLES 4
  GL.multisample $= GL.Enabled

  GL.viewport $= (GL.Position 0 0, GL.Size (fromIntegral width) (fromIntegral height))
  liftIO $ GLU.ortho2D 0 (fromIntegral width) (fromIntegral height) 0

  let vertexBufferData = [ 100, 100
                         , 300, 100
                         , 100, 300::GL.GLfloat ]
  vbo <- liftIO $ GLUtil.makeBuffer GL.ArrayBuffer vertexBufferData
  vs <- liftIO $ GLUtil.loadShader GL.VertexShader "hello-gl.vert"
  fs <- liftIO $ GLUtil.loadShader GL.FragmentShader "hello-gl.frag"
  p <- liftIO $ GLUtil.linkShaderProgram [vs, fs]

  kr <- liftIO $ newIORef (Set.empty)
  return $ GLResources { vertBuffObj = vbo
                       , program = p
                       , sdlglContext = context
                       , sdlWindow = window
                       , keysRef = kr
                       }
    where initialWindow =
            SDL.defaultWindow { SDL.windowOpenGL = Just SDL.defaultOpenGL
                              , SDL.windowInitialSize = size }
          size = V2 (fromIntegral width) (fromIntegral height)

type GLRender m = m ()

finishSDL_GL :: (MonadIO m) => GLResources -> GLRender m
finishSDL_GL r = do
  liftIO GL.finish
  SDL.glDeleteContext (sdlglContext r)

senseSDL_GL :: (MonadIO m) => GLResources -> m (Set Key)
senseSDL_GL r = senseSDL ((), keysRef r)

actuateSDL_GL :: (MonadIO m) => Double -> Double -> GLResources -> GLRender m -> m ()
actuateSDL_GL width height r drawAction = do
  -- Clear back buffer to black.
  liftIO $ GL.clearColor $= GL.Color4 0 0 0 1
  liftIO $ GL.clear [GL.ColorBuffer]
  -- Perform the drawing action.
  drawAction
  -- Swap buffers.
  liftIO $ SDL.glSwapWindow (sdlWindow r)

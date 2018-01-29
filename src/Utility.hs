{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}

module Utility where

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Monad.Random
import Data.Either
import Data.Traversable
import Data.Foldable
import System.Random

import Linear.Vector
import Linear.Matrix
import Linear.Metric
import Linear.Epsilon
import Linear.V1
import Linear.V2

import Control.Lens

import Control.Wire
import Control.Wire.Unsafe.Event

import Manifold
import UniqueMap (UMap)
import qualified UniqueMap as UMap

--------------------------------------------------------------------------------
-- Wrapped Manifold types
--------------------------------------------------------------------------------

newtype Angle a = Angle { unAngle :: V1 a }
                deriving (Eq, Show)

instance (RealFrac a, Floating a) => Manifold.Wrapped (Angle a) where
  type RepS (Angle a) = a
  type RepV (Angle a) = V1
  wrapRange = ((V1 (-pi)), (V1 pi))
  fromWrapped = unAngle
  toWrapped = Angle . wrapV ((V1 (-pi)), (V1 pi))

newtype ScreenPos a = ScreenPos { unScreenPos :: V2 a }
                    deriving (Eq, Show)

width = 600
height = 600

instance (RealFrac a, Floating a) => Manifold.Wrapped (ScreenPos a) where
  type RepS (ScreenPos a) = a
  type RepV (ScreenPos a) = V2
  wrapRange = ((V2 0 0), (V2 (fromIntegral width) (fromIntegral height)))
  fromWrapped = unScreenPos
  --  For some reason the following doesn't work:
  --  toWrapped = ScreenPos . wrapV wrapRange
  toWrapped = ScreenPos . wrapV ((V2 0 0), (V2 (fromIntegral width) (fromIntegral height)))

--------------------------------------------------------------------------------
-- Integral functions for Netwire
--------------------------------------------------------------------------------

-- Integral for vector values using Euler integration.
integralV :: (Additive v, Fractional a, HasTime t s)
          => (v a) -> Wire s e m (v a) (v a)
integralV v0 = mkPure $ \ds dv ->
  let dt = realToFrac (dtime ds)
  in v0 `seq` (Right v0, integralV (v0 ^+^ dt*^dv))
  
-- Integral for Manifold values using Euler integration.
integralM :: (Manifold n, Fractional (Scalar n), HasTime t s)
          => n -> Wire s e m (Tangent n (Scalar n)) n
integralM n0 = mkPure $ \ds dn ->
  let dt = realToFrac (dtime ds)
  in n0 `seq` (Right n0, integralM (n0 .+~^ (dt*^dn)))

-- Integral for relativistic velocities using Euler integration.
integralRelV :: (Metric v, Additive v, Floating a, Epsilon a, HasTime t s)
          => (v a) -> Wire s e m (v a) (v a)
integralRelV v0 = mkPure $ \ds dv ->
  let dt = realToFrac (dtime ds)
  in v0 `seq` (Right v0, integralRelV (relAddVel v0 (dt*^dv)))
  
--------------------------------------------------------------------------------
-- Netwire utilities
--------------------------------------------------------------------------------

stepWires :: (Monoid s, Monad m)
          => Wire s e m (a, UMap (Wire s e m a b)) (UMap (b, Wire s e m a b))
stepWires = mkGen f
  where f s (a, ws) = do
          autos <- mapM (\w -> stepWire w s (Right a)) ws
          let autos' = UMap.rights $
                       UMap.map (\(x, ws) -> case x of
                                    Right y -> Right (y, ws)
                                    Left y  -> Left (y, ws))
                       autos
          return (Right autos', stepWires)

-- Dynamic collection of wires starting with the given initial set of
-- wires.  Input is mapped to each wire and the outputs are collected
-- as a list.  The resulting wire also takes an event, which contains
-- a function to modify the list (dynamically add or remove wires).
-- TODO: Consider removing any wire that has inhibited.
collectWires
  :: (MonadFix m, Monoid s)
  => UMap (Wire s e m a b)
  -> Wire s e m (a, Event (UMap (Wire s e m a b) -> UMap (Wire s e m a b))) (UMap b)
collectWires ws = dSwitch initWire
  where initWire = proc (input, modifyWiresE) -> do
          rec
            autos <- stepWires . second (delay ws) -< (input, ws')
            let (outputs, ws') = (fst <$> autos, snd <$> autos)
          let newWireE = collectWires . ($ ws') <$> modifyWiresE
          returnA -< (outputs, newWireE)
{-
collectWires
  :: (MonadFix m)
  => [Wire s e m a b]
  -> Wire s e m (a, Event ([(b, Wire s e m a b)] -> [Wire s e m a b])) [b]
collectWires ws = proc (input, modifyWiresE) -> do
  rec
    let newWireE = sequenceA . ($ zip outputs ws) <$> modifyWiresE
    outputs <- drSwitch (sequenceA ws) -< (input, newWireE)
  returnA -< outputs

collectWires
  :: (MonadFix m)
  => UMap (Wire s e m a b)
  -> Wire s e m (a, Event (UMap (Wire s e m a b) -> UMap (Wire s e m a b))) (UMap b)
collectWires ws = dSwitch initWire
  where initWire = proc (input, modifyWiresE) -> do
          outputs <- sequenceA ws -< input
          let newWireE = collectWires . ($ ws) <$> modifyWiresE
          returnA -< (outputs, newWireE)
-}

mergeAll :: (a -> a -> a) -> [Event a] -> Event a
mergeAll f = foldr (merge f) NoEvent

-- Fires an event every time the wire is stepped and the input
-- satisfies the given predicate.
eventsWhen :: (Monad m, Monoid e) => (a -> Bool) -> Wire s e m a (Event a)
eventsWhen p = proc x -> do
  if p x
  then returnA -< Event x
  else returnA -< NoEvent

-- Applies a random number to the given function.
randomE :: (MonadRandom m, Random b) => Wire s e m (Event (b -> a)) (Event a)
randomE = mkGen_ $ \e -> do
  r <- getRandom
  return $ Right (($ r) <$> e)
  
{- CAUSED ERROR: "thread blocked indefinitely in an MVar operation"
-- Each time an event occurs, produces and holds a randomly generated
-- value.
randomW :: (MonadRandom m, Monoid s, Random b)
        => b -> Wire s e m (Event a) b
randomW x0 = mkGen $ \_ e ->
  case e of
    NoEvent -> return (Right x0, randomW x0)
    Event _ -> do
      r <- getRandom
      return (Right x0, randomW r)
-}

-- Analogous to randomR like randomE is to random.
randomRE :: (MonadRandom m, Random b) => (b, b) -> Wire s e m (Event (b -> a)) (Event a)
randomRE (r0, r1) = mkGen_ $ \e -> do
  r <- getRandomR (r0, r1)
  return $ Right (($ r) <$> e)

randomRW :: (MonadRandom m, Monoid s, Random b)
         => b -> (b, b) -> Wire s e m (Event a) b
randomRW x0 (r0, r1) = mkGen $ \_ e ->
  case e of
    NoEvent -> return (Right x0, randomRW x0 (r0, r1))
    Event _ -> do
      r <- getRandomR (r0, r1)
      return (Right r, randomRW r (r0, r1))
      
-- Analogous to randomRs like randomE is to random.
randomRsE :: (MonadRandom m, Random b) => (b, b) -> Wire s e m (Event ([b] -> a)) (Event a)
randomRsE (r0, r1) = mkGen_ $ \e -> do
  rs <- getRandomRs (r0, r1)
  return $ Right (($ rs) <$> e)

randomRsW :: (MonadRandom m, Monoid s, Random b)
          => [b] -> (b, b) -> Wire s e m (Event a) [b]
randomRsW xs (r0, r1) = mkGen $ \_ e ->
  case e of
    NoEvent -> return (Right xs, randomRsW xs (r0, r1))
    Event _ -> do
      rs <- take 10 <$> getRandomRs (r0, r1)
      return (Right rs, randomRsW xs (r0, r1))
{-
-- Delay every incoming event by t seconds.
delayE :: (HasTime t s) => t -> Wire s e m (Event a) (Event a)
delayE t = switch delayE'
  where delayE' = proc inputE -> do
          outputE <- FRP.Netwire.at t
-}

steps :: (MonadIO m) => Session m s -> Wire s () m a b -> [a] -> m [b]
steps session wire inputs = steps' session wire inputs []
  where steps' session wire inputs outputs = do
          liftIO $ putStrLn "Stepping session."
          (stateDelta, session') <- stepSession session
          let input = if null inputs then Left () else Right (head inputs)   
          liftIO $ putStrLn "Stepping wire."       
          (output, wire') <- stepWire wire stateDelta input
          case output of
            Left _ -> return outputs
            Right o -> steps' session' wire' (tail inputs) (outputs ++ [o])

--------------------------------------------------------------------------------
-- Matrix functions
--------------------------------------------------------------------------------

rotationMatrix :: (Floating a) => a -> M22 a
rotationMatrix theta = V2 (V2 (cos theta) (-(sin theta)))
                          (V2 (sin theta) (cos theta))

scaleMatrix :: (Num a) => V2 a -> M22 a
scaleMatrix (V2 x y) = V2 (V2 x 0)
                          (V2 0 y)

--------------------------------------------------------------------------------
-- Relativity functions
--------------------------------------------------------------------------------

-- Speed of light in px/s.
c :: Num a => a
c = 100

-- TODO: Returns NaN if |v|^2 > c^2.
gamma :: (Metric f, Floating a) => f a -> a
gamma v = 1/sqrt(1 - (quadrance v / c^2))

-- Compose two velocities relativistically.
relAddVel :: (Metric f, Additive f, Floating a, Epsilon a)
          => f a -> f a -> f a
relAddVel v0 v1 | nearZero v02 = v1
                | otherwise = 1/(1 + v0`dot`v1/c^2)
                              *^ (v1^/g0 ^+^ v0^*(1+(1-1/g0)*(v0`dot`v1/v02)))
  where g0 = gamma v0
        v02 = quadrance v0

-- Creates a matrix that scales by a factor of 1/gamma in the
-- direction of the given velocity.  Returns the identity function if
-- the velocity is near enough to zero.
relContractM :: (Floating a, Epsilon a) => V2 a -> M22 a
relContractM vel =
  if nearZero vel2
  then identity
  -- else transpose (V2 vel_ (perp vel_))
  --  !*! V2 (V2 (1/gamma vel) 0) (V2 0 1)
  --  !*! V2 vel_ (perp vel_)
  else V2 (V2 (x^2/g + y^2) (x*y/g - x*y))
          (V2 (x*y/g - x*y) (y^2/g + x^2)) !!/ vel2
  where vel2 = quadrance vel
        V2 x y = vel
        g = gamma vel


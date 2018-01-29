{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-} -- Should be able to get rid of this.
module Wrapped where

import Linear.Vector
import Linear.V1

{-class Wrapped a where
  -- 'fromWrapped w' converts a wrapped number 'w' to a rational number
  -- in the range [-0.5,0.5).
  fromWrapped :: Fractional b => a -> b
  -- 'toWrapped x' converts a number 'x' in the range [-0.5,0.5) to a
  -- wrapped number.
  toWrapped   :: Real b => b -> a
-}
{-
class (Num (S w), Additive (V w)) => Wrapped w where
  type S w :: *
  type V w :: * -> *
  wrapRange :: (V w (S w), V w (S w))
  fromWrapped :: w -> V w (S w)
  toWrapped :: V w (S w) -> w

newtype Angle a = Angle { unAngle :: a }
                deriving (Eq, Show)

instance Floating a => Wrapped (Angle a) where
  type S (Angle a) = a
  type V (Angle a) = V1
  wrapRange = (V1 (-pi), V1 pi)
  fromWrapped = V1 . unAngle
  toWrapped = Angle . wrapV wrapRange
-}

class Wrapped w where
  type Rep w :: *
  wrapRange :: (Rep w, Rep w)
  fromWrapped :: w -> Rep w
  toWrapped :: Rep w -> w
{-
newtype Angle a = Angle { unAngle :: a }
                deriving (Eq, Show)

instance (RealFrac a, Floating a) => Wrapped (Angle a) where
  type Rep (Angle a) = a
  wrapRange = (realToFrac (-pi), realToFrac pi)
  fromWrapped = unAngle
  toWrapped = Angle . wrap wrapRange
-}

newtype Angle = Angle { unAngle :: Double }
              deriving (Eq, Show)

instance Wrapped Angle where
  type Rep Angle = Double
  wrapRange = (-pi, pi)
  fromWrapped = unAngle
  toWrapped = Angle . wrap (-pi, pi)

-- Wraps vector v to the hypercube defined by two opposite corners
-- minV and maxV.  No two coordinates of minV and maxV may be the
-- same, or else there could be a divide by zero.
wrapV :: (RealFrac a, Additive f) => (f a, f a) -> f a -> f a
wrapV (v0, v1) v = v ^-^ (liftI2 (*) c dv)
  where c  = fromIntegral . ceiling <$> liftI2 (/) (v ^-^ v1) dv
        dv = v1 ^-^ v0

wrap :: RealFrac a => (a, a) -> a -> a
wrap (x0, x1) x = x - c*dx
  where c  = fromIntegral $ ceiling $ (x - x1) / dx
        dx = x1 - x0

-- Wraps x to the interval [0, 1).
wrapUnit x = x - c
  where c = fromIntegral (floor x)

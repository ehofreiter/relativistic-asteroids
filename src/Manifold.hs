{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Manifold where

import Linear.Vector
import Control.Lens hiding (Wrapped)

-- TODO: Make manifold parameterized by the tangent space.  The
-- codomain of charts is the underlying vector space, hence the
-- parameters, or coordinates, that represent the domain should have
-- the same type as the tangent space.  Ie, for a manifold M with
-- charts phi : U -> E, where E is a vector space, the tangent bundle
-- TM has charts Tphi : TU -> TE, where TE =~ (E, E).  Tphi is a
-- vector bundle isomorphism, so T_mphi : T_mU =~ E is an isomorphism
-- of the tangent space at m T_mU to E.

class (Num (Scalar m), Additive (Tangent m)) => Manifold m where
  type Scalar m  :: *
  type Tangent m :: * -> *
  (.+~^) :: m -> Tangent m (Scalar m) -> m
--  m .+~^ v = fromParams (params m ^+^ v)
  (.-~^) :: m -> Tangent m (Scalar m) -> m
  m .-~^ v = m .+~^ (negated v)
--  params :: m -> Tangent m (Scalar m)
--  fromParams :: Tangent m (Scalar m) -> m

{-
-- Datatype for a circle parameterized by a number.
data S1 a = S1 a
          deriving (Eq, Show)

instance RealFrac a => Manifold (S1 a) where
  type Scalar (S1 a)  = a
  type Tangent (S1 a) = V1
  params (S1 x) = V1 x
  fromParams (V1 x) = S1 (wrap (-0.5, 0.5) x)

-- Datatype for a torus parameterized by two numbers.
data T2 a = T2 a a
          deriving (Eq, Show)

instance RealFrac a => Manifold (T2 a) where
  type Scalar (T2 a)  = a
  type Tangent (T2 a) = V2
  params (T2 x y) = V2 x y
  fromParams (V2 x y) =
    T2 (wrap (-0.5, 0.5) x) (wrap (-0.5, 0.5) y)
-}
wrap :: RealFrac a => (a, a) -> a -> a
wrap (x0, x1) x = x - c*dx
  where c  = fromIntegral $ ceiling $ (x - x1) / dx
        dx = x1 - x0

-- Wraps vector v to the hypercube defined by two opposite corners
-- minV and maxV.  No two coordinates of minV and maxV may be the
-- same, or else there could be a divide by zero.
wrapV :: (RealFrac a, Additive f) => (f a, f a) -> f a -> f a
wrapV (v0, v1) v = v ^-^ (liftI2 (*) c dv)
  where c  = fromIntegral . ceiling <$> liftI2 (/) (v ^-^ v1) dv
        dv = v1 ^-^ v0

-- A wrapped vector space (homeomorphic to a torus).
class (Num (RepS w), Additive (RepV w)) => Wrapped w where
  type RepS w :: *
  type RepV w :: * -> *
  -- Two opposing corners of a hypercube in the representative vector
  -- space.  (fromWrapped . toWrapped) v must lie within this
  -- hypercube.
  wrapRange :: (RepV w (RepS w), RepV w (RepS w))
  fromWrapped :: w -> RepV w (RepS w)
  toWrapped :: RepV w (RepS w)-> w

rep :: (Wrapped w) => Lens' w (RepV w (RepS w))
rep = lens fromWrapped (\w r -> toWrapped r)

instance Wrapped w => Manifold w where
  type Scalar w = RepS w
  type Tangent w = RepV w
  m .+~^ v = toWrapped (fromWrapped m ^+^ v)

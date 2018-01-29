{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module AsteroidsGame
    ( main_
    ) where

import Prelude hiding ((.), id, until)

import Data.List hiding (transpose)
import Data.Functor
import Data.Monoid
import Control.Monad.IO.Class
import Control.Monad.Fix
import Control.Monad.Random hiding (fromList)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector.Storable (fromList)

import Linear.Vector
import Linear.Affine
import Linear.Matrix
import Linear.Metric
import Linear.Epsilon
import Linear.V1
import Linear.V2
import Linear.V4

import Control.Lens

import qualified SDL
import SDL (($=))

import qualified Graphics.Rendering.OpenGL.GL as GL

import FRP.Netwire
import Control.Wire.Unsafe.Event

import Run
import Key
import Manifold
import UniqueMap (UMap, (!))
import qualified UniqueMap as UMap
import Utility

type Time = Double

data Ship = Ship { _shipRot :: Angle Double
                 , _shipPos :: ScreenPos Double
                 , _shipVel :: V2 Double
                 , _shipTime :: Time
                 }

data Bullet = Bullet { _bulPos :: ScreenPos Double
                     , _bulTime :: Time
                     , _bulDeathE :: Event () -- Event to kill this
                                             -- bullet when it runs out of
                                             -- time.
                     }

data Asteroid = Asteroid { _astPos :: ScreenPos Double
                         , _astVel :: V2 Double
                         , _astRot :: Angle Double
                         , _astSize :: Int -- An asteroid of size n
                                          -- will break into 3
                                          -- asteroids of size (n-1).
                                          -- Size 1 asteroids are
                                          -- simply destroyed.
                         }

makeLenses ''Ship
makeLenses ''Bullet
makeLenses ''Asteroid

main_ :: IO ()
main_ = run (initSDL_GL "Asteroids" width height)
            finishSDL_GL
            senseSDL_GL
            (actuateSDL_GL (fromIntegral width) (fromIntegral height))
            game

game :: (MonadRandom m, MonadIO m, MonadFix m, HasTime t s)
     => Wire s () m (Set Key) (GLRender m)
game = switch gameRound

gameRound :: (MonadRandom m, MonadIO m, MonadFix m, HasTime t s)
          => Wire s () m (Set Key) (GLRender m, Event (Wire s () m (Set Key) (GLRender m)))
gameRound = proc keys -> do
  -- Create an inhibiting wire when a quit event happens, causing the
  -- window to close (crash?).
  until . second quitE -< ((), keys)

  -- Ship
  ship <- shipA -< keys

  rec
    -- Bullets
    shootBulletE <- triggerE -< keys
    let newBulletA = bulletA ((ship^.shipPos)
                              .+~^ (shipScale/gamma (ship^.shipVel)
                                    *^ angle (ship^.shipAngle)))
                             (ship^.shipVel)
                             (ship^.shipAngle)
    let createBulletE = UMap.insert newBulletA <$ shootBulletE
    let expiredBulletEs = map (\(k, bul) -> UMap.delete k <$ bul^.bulDeathE) $ UMap.assocs bullets
    let bulletChangesE = mergeAll (.) $ [createBulletE] ++ [killBulletsE] ++ expiredBulletEs
    bullets <- collectWires UMap.empty -< ((), bulletChangesE)

    -- Asteroids
    -- TODO: Remove the "NoEvent" delay somehow.
    newAsteroidsE <- splitAsteroids . delay NoEvent -< deadAsteroidsE
    let createAsteroidsE = foldr (.) id . map UMap.insert <$> newAsteroidsE
    let asteroidChangesE = mergeAll (.) [createAsteroidsE, killAsteroidsE]
    asteroids <- collectWires (UMap.fromList (initialAsteroids 4)) -< ((), asteroidChangesE)

    -- Collisions
    let bulsVsAsts = (,) <$> UMap.assocs bullets
                       <*> UMap.assocs asteroids
    let colliding = filter (\((kb, b), (ka, a)) ->
                             intersectingWrapped (shape b) (shape a))
                           bulsVsAsts
    let deadBulletKs = map (fst.fst) colliding
    let deadAsteroidKs = map (fst.snd) colliding
    deadAsteroidsE <- eventsWhen (not . null) -< map (snd.snd) colliding
    killBulletsE <- arr (UMap.deleteKeys <$>) . eventsWhen (not . null) -< deadBulletKs
    killAsteroidsE <- arr (UMap.deleteKeys <$>) . eventsWhen (not . null) -< deadAsteroidKs

    let shipDead = any (intersecting (shape ship) . shape) $ UMap.toList asteroids
    restartE <- became id -< shipDead
    
  -- Output
  let render =  do renderShip ship
                   mapM_ (\b -> renderBullet b) bullets
                   mapM_ (\a -> renderAsteroid a) asteroids

  returnA -< (render, game <$ restartE)

{-
splitAsteroids :: (MonadRandom m, Monoid e, HasTime t s)
               => Wire s e m (Event [Asteroid]) (Event [Wire s e m a Asteroid])
splitAsteroids = proc splitAstsE -> do
  newAstsE <- randomRsE (5, 30)  -- speed2
            . randomRsE (5, 30)  -- speed1
            . randomRsE (0, 2*pi) -- theta2
            . randomRsE (0, 2*pi) -- theta1
              -< zipWith5 mkAsts <$> splitAstsE
  returnA -< concat <$> newAstsE
    where mkAsts ast t1 t2 s1 s2 =
            if ast^.astSize > 1
            then
              let vel1 = s1 *^ angle t1
                  vel2 = s2 *^ angle t2
                  vel3 = negated vel1 ^-^ vel2
              in map (\v ->
                       asteroidA (ast^.astSize - 1)
                                 (ast^.astPos)
                                 (ast^.astVel ^+^ v))
                     [vel1, vel2, vel3]
            else []
-}

splitAsteroids :: (MonadRandom m, Monoid e, HasTime t s)
               => Wire s e m (Event [Asteroid]) (Event [Wire s e m a Asteroid])
splitAsteroids = proc splitAstsE -> do
  theta1 <- randomRsW [] (0, 2*pi) -< splitAstsE
  theta2 <- randomRsW [] (0, 2*pi) -< splitAstsE
  speed1 <- randomRsW [] (5, 30) -< splitAstsE
  speed2 <- randomRsW [] (5, 30) -< splitAstsE
  rotV1 <- randomRsW [] (0, pi) -< splitAstsE
  rotV2 <- randomRsW [] (0, pi) -< splitAstsE
  let newAstsE = zipWith7 mkAsts theta1 theta2 speed1 speed2 rotV1 rotV2 <$> splitAstsE
  returnA -< concat <$> newAstsE
    where mkAsts t1 t2 s1 s2 rv1 rv2 ast =
            if ast^.astSize > 1
            then
              let vel1 = s1 *^ angle t1
                  vel2 = s2 *^ angle t2
                  vel3 = negated vel1 ^-^ vel2
                  rv3 = - rv1 + rv2
              in map (\(v, rv) ->
                       asteroidA (ast^.astSize - 1)
                                 (ast^.astPos)
                                 (ast^.astVel ^+^ v)
                                 (V1 rv))
                     [(vel1, rv1), (vel2, rv2), (vel3, rv3)]
            else []

-- initialAsteroids :: (MonadRandom m, Monoid e, HasTime t s)
--                  => Int -> Wire s e m (Event a) (Event [Wire s e m a Asteroid])
-- initialAsteroids round = proc e -> do
--   rot <- randomRsW [] (0, 2*pi) -< e -- angle of rotation
--   x <- randomRsW [] (0, fromIntegral width) -< e
--   y <- randomRsW [] (0, fromIntegral height) -< e
--   let newAsts = zipWith5 (\s x' y' v rv -> asteroidA s (ScreenPos (V2 x' y')) v (V1 rv))
--                          (replicate round 4)
--                          x y (repeat (V2 0.0 0.0)) rot
--   returnA -< newAsts <$ e

initialAsteroids :: (Monad m, Monoid e, HasTime t s)
                 => Int -> [Wire s e m a Asteroid]
initialAsteroids round = [ asteroidA 4 (ScreenPos (V2 100 100)) (V2 0 0) (V1 pi)
                         , asteroidA 4 (ScreenPos (V2 500 100)) (V2 0 0) (V1 (-pi))
                         , asteroidA 4 (ScreenPos (V2 100 600)) (V2 0 0) (V1 (pi/2))]

--------------------------------------------------------------------------------
-- Player-controlled ship
--------------------------------------------------------------------------------

thrustRate = 100 -- px/s^2
turnRate = 3 -- rad/s
shipScale = 10

shipA :: (MonadFix m, HasTime t s, Monoid e) => Wire s e m (Set Key) Ship
shipA = proc keys -> do
  rec
    rot <- integralM (Angle $ V1 (-pi/2)) -< V1 (turn keys)
    let Angle (V1 rot') = rot
    --  let accel = thrust keys *^ angle rot'
    let dir = angle (rot^.rep._x)
    let properAccel = thrust keys
    let g = gamma vel
    let vel2 = quadrance vel
    let accel = if nearZero vel2
                then properAccel *^ dir
                else properAccel
                     *^ ((1/g^2) *^ dir
                         ^-^ ((g-1)/(g^3))*(vel `dot` dir)*^vel ^/ vel2)
    vel <- integralV zero -< accel
--    vel <- integralRelV zero -< properAccel *^ dir
    pos <- integralM initialPos -< vel
    properTime <- integral 0 -< 1 / g
  returnA -< Ship { _shipRot = rot
                  , _shipPos = pos
                  , _shipVel = vel
                  , _shipTime = properTime }
    where initialPos = ScreenPos $ ((/ 2) . fromIntegral) <$> V2 width height
          
-- Lens for the angle that the ship is facing as a double.
shipAngle :: Lens' Ship Double
shipAngle = shipRot . rep . _x

instance HasShape Ship where
  shape ship = PolygonS $ shipPolygon ship (ship^.shipPos.rep)

shipPolygon :: Ship -> V2 Double -> Polygon
shipPolygon ship pos = map ((pos ^+^) . (relConM !*! rotM !*! scaleM !*)) points
  where rotM = rotationMatrix (ship^.shipAngle)
        scaleM = scaled (shipScale *^ signorm (V2 2 1))
        relConM = relContractM (ship^.shipVel)
        points = regularPolygon 3

-- Color flashes based on proper time of ship.
renderShip :: (MonadIO m) => Ship -> GLRender m
renderShip ship = renderWrapped renderShip' (ship^.shipPos)
  where renderShip' v = renderPolyline (GL.Color3 t 1 t) $ shipPolygon ship v
        t = cos (ship^.shipTime * pi / pt)
        pt = 0.5 -- Period of flashing (in seconds)

--------------------------------------------------------------------------------
-- Bullets
--------------------------------------------------------------------------------

-- TODO: Speed should really be defined in natural units and should be
-- restricted to the unit interval [0,1], which is a manifold with
-- boundary.  Alternatively, could specify as hyperbolic angle
-- (rapidity) instead.
bulletSpeed = 0.9*c

-- This is the minimum amount of time the bullet will be alive for (if
-- speed is zero).  Coordinate time t that the bullet lives for is t =
-- gamma * tau
bulletLife = 2 -- s (proper time)

bulletA :: (Monad m, HasTime t s)
        => ScreenPos Double -> V2 Double -> Double -> Wire s e m a Bullet
bulletA initPos launchVel dir = proc _ -> do
  let vel = launchVel `relAddVel` (bulletSpeed *^ angle dir)
  pos <- integralM initPos -< vel
  t <- time -< ()
  let properTime = realToFrac t / gamma vel
  deathE <- arr void . became (> bulletLife) -< properTime
  returnA -< Bullet { _bulPos = pos
                    , _bulTime = properTime
                    , _bulDeathE = deathE }

instance HasShape Bullet where
  shape bul = PointS (bul^.bulPos.rep)

-- Bullets start white and turn red as they get closer to death.
renderBullet :: (MonadIO m) => Bullet -> GLRender m
renderBullet bullet = renderPoint (GL.Color3 1 t t) (bullet^.bulPos.rep)
  where t = 1 - bullet^.bulTime/bulletLife

--------------------------------------------------------------------------------
-- Asteroids
--------------------------------------------------------------------------------

asteroidA :: (Monad m, HasTime t s)
          => Int -> ScreenPos Double -> V2 Double -> V1 Double -> Wire s e m a Asteroid
asteroidA size initPos vel rotV = proc _ -> do
  pos <- integralM initPos -< vel
  rot <- integralM (Angle (V1 0)) -< rotV
  returnA -< Asteroid { _astPos = pos
                      , _astVel = vel
                      , _astRot = rot
                      , _astSize = size }

-- Lens for the angle of the given Asteroid as a Double.
astAngle :: Lens' Asteroid Double
astAngle = astRot . rep . _x

instance HasShape Asteroid where
  shape ast = PolygonS $ asteroidPolygon ast (ast^.astPos.rep)

asteroidPolygon :: Asteroid -> V2 Double -> Polygon
asteroidPolygon ast v = map ((v ^+^) . (relConM !*) . (rotM !*) . (scaleM !*)) points
        -- The scaling factor is the radius of the asteroid.  We want
        -- the area of a size n asteroid to be equal to the area of 3
        -- size (n-1) asteroids.  We also want the radius of a size 1
        -- (triangle) asteroid to be 10 px.
        -- A(n) = 3 * A(n-1)
        -- A(n) = pi * r(n)^2
        -- pi * r(n)^2 = 3 * pi * r(n-1)^2)
        -- r(n) = sqrt(3) * r(n-1)
        -- r(1) = 10
        -- r(n) = sqrt(3)^(n-1) * 10
    where scaleM = scaleMatrix $ sqrt(3)^(ast^.astSize - 1) * 10
          rotM = rotationMatrix $ ast^.astAngle
          relConM = relContractM (ast^.astVel)
          points = regularPolygon (ast^.astSize + 2)

renderAsteroid :: (MonadIO m) => Asteroid -> GLRender m
renderAsteroid asteroid = renderWrapped renderAsteroid' (asteroid^.astPos)
  where renderAsteroid' v =
          renderPolyline cWhite $ asteroidPolygon asteroid v


--------------------------------------------------------------------------------
-- Input functions
--------------------------------------------------------------------------------

thrust :: Set Key -> Double
thrust keys = if Set.member KeyUp keys
              then thrustRate
              else 0

turn :: Set Key -> Double
turn keys = case (Set.member KeyLeft keys, Set.member KeyRight keys) of
  (True, False) -> -turnRate
  (False, True) -> turnRate
  otherwise -> 0

-- Triggers an event when the given key is pressed.
keyDownE :: (Monad m) => Key -> Wire s e m (Set Key) (Event ())
keyDownE k = arr void . became (Set.member k)

triggerE :: (Monad m) => Wire s e m (Set Key) (Event ())
triggerE = keyDownE KeySpace

quitE :: (Monad m) => Wire s e m (Set Key) (Event ())
quitE = keyDownE KeyEscape

--------------------------------------------------------------------------------
-- Collision Detection
--------------------------------------------------------------------------------

class HasShape a where
  shape :: a -> Shape

data Shape = PolygonS Polygon
           | LineSegS (V2 Double, V2 Double)
           | PointS (V2 Double)

-- A polygon is represented by a list of points in counter-clockwise
-- order (counter-clockwise assuming the +x-axis points to the right
-- and the +y-axis points up).
type Polygon = [V2 Double]

-- Returns a list of the polygon edges.  Each edge is a line segment
-- represented as a pair of points.
polyEdges :: Polygon -> [(V2 Double, V2 Double)]
polyEdges ps = zip ps ps'
  where ps' = tail ps ++ [head ps]

-- Returns a list of outward-facing normals for the polygon.  This
-- requires the polygon points to be in counter-clockwise (ccw) order.
-- If the polygon points are labeled [p0, p1, ... , pn], then the
-- first normal is the normal of edge (p0, p1).  Since the points are
-- in ccw order, the normal is the clockwise (cw) perpendicular
-- rotation of (pn - p(n-1)).  The 'perp' function is the ccw
-- rotation, so we take the negative of 'perp'.
polyNormals :: Polygon -> [V2 Double]
polyNormals = map (signorm . negated . perp . (\(p0, p1) -> p1 ^-^ p0)) . polyEdges

data Interval a = EmptyI
                | Interval a a
                  deriving (Eq, Ord, Show)

emptyI :: Interval a -> Bool
emptyI EmptyI = True
emptyI _      = False

-- Intersection of two intervals.
intervalI :: (Ord a) => Interval a -> Interval a -> Interval a
intervalI EmptyI _ = EmptyI
intervalI _ EmptyI = EmptyI
intervalI (Interval x0 x1) (Interval y0 y1)
  | x1 < y0 || y1 < x0 = EmptyI
  | otherwise          = Interval (max x0 y0) (min x1 y1)

-- Normalize the interval (convert to EmptyI if applicable).
normI i@(Interval x0 x1) | x1 <= x0  = EmptyI
                         | otherwise = i

intersectingWrapped :: Shape -> Shape -> Bool
intersectingWrapped s1 (PolygonS ps) = or $ map (intersecting s1) s2s
  where s2s = map (\z -> PolygonS $ map (z^+^) ps) wrappedZeros
intersectingWrapped s1 (LineSegS (v1, v2)) = or $ map (intersecting s1) s2s
  where s2s = map (\z -> LineSegS (z^+^v1, z^+^v2)) wrappedZeros
intersectingWrapped s1 (PointS v) = or $ map (intersecting s1) s2s
  where s2s = map (\z -> PointS (z^+^v)) wrappedZeros

intersecting :: Shape -> Shape -> Bool
intersecting (PolygonS ps) (PointS a) =
  and $ zipWith insideEdge ps (polyNormals ps)
    where insideEdge p n = (p-a) `dot` n >= 0
intersecting x@(PointS _) y@(PolygonS _) = intersecting y x
intersecting (PolygonS ps) (LineSegS (a, b)) =
  not $ emptyI $ foldr intervalI (Interval 0 1) $ zipWith insideEdge ps (polyNormals ps)
  where insideEdge p n =
          if nearZero ab_n
          then if pb_n >= 0 then Interval 0 1 else EmptyI
          else if ab_n > 0
               -- These pairs are intervals that represent the part of
               -- the line segment that is inside the poly edge.  The
               -- entire line segment spans (0, 1) and is given by the
               -- parameterization l(r) = b + r*(a-b).
               then Interval 0 r
               else Interval r 1
            where ab_n = (a-b) `dot` n
                  pb_n = (p-b) `dot` n
                  r = pb_n / ab_n
intersecting x@(LineSegS _) y@(PolygonS _) = intersecting y x
intersecting p1@(PolygonS _) (PolygonS ps2) =
  or $ map (intersecting p1 . LineSegS) (polyEdges ps2)
intersecting _ _ = False

--------------------------------------------------------------------------------
-- Rendering functions
--------------------------------------------------------------------------------

cBlack, cWhite, cGray, cRed, cGreen, cBlue :: GL.Color3 GL.GLdouble
cBlack = GL.Color3 0 0 0
cWhite = GL.Color3 1 1 1
cGray  = GL.Color3 0.5 0.5 0.5
cRed   = GL.Color3 1 0.25 0.25
cGreen = GL.Color3 0.25 1 0.25
cBlue  = GL.Color3 0.25 0.25 1

-- Render several shifted copies of an image onto a wrapped screen.
-- This way, if e.g. the image is cut off by the left side of the
-- screen, the rest will appear on the right side of the screen.
renderWrapped :: (MonadIO m, v ~ Tangent (ScreenPos a), s ~ (Scalar (ScreenPos a)), Num s)
              => (v s -> GLRender m) -> ScreenPos a -> GLRender m
-- renderWrapped render pos = mapM_ (\p -> render p) vs
--   where vs = map (v ^+^) [V2 x' y' | x' <- [-x, 0, x], y' <- [-y, 0, y]]
--         v = unScreenPos pos
--         x = fromIntegral width
--         y = fromIntegral height
renderWrapped render pos = mapM_ render $ map ((unScreenPos pos) ^+^) wrappedZeros

wrappedZeros :: (Num a) => [V2 a]
wrappedZeros = [V2 x' y' | x' <-  [-x, 0, x], y' <- [-y, 0, y]]
  where x = fromIntegral width
        y = fromIntegral height

renderPoint :: (MonadIO m, Real a, GL.Color c)
            => c -> V2 a -> GLRender m
renderPoint c v = liftIO $ GL.renderPrimitive GL.Points $ do
  GL.color c
  GL.vertex (vecToVert v)

renderPolyline :: (MonadIO m, Real a, GL.Color c)
               => c -> [V2 a] -> GLRender m
renderPolyline c vs = liftIO $ GL.renderPrimitive GL.LineLoop $ do
  GL.color c
  mapM_ (GL.vertex . vecToVert) vs

renderNothing :: (MonadIO m) => GLRender m
renderNothing = return ()
                        
regularPolygon :: Int -> Polygon
regularPolygon n | n < 3     = []
                 | otherwise = map (angle . (* step) . fromIntegral) [0..(n-1)]
  where step = 2*pi / fromIntegral n

vecToVert :: (Real a) => V2 a -> GL.Vertex2 GL.GLdouble
vecToVert (V2 x y) = GL.Vertex2 (realToFrac x) (realToFrac y)

{-# LANGUAGE RecordWildCards #-}

module Ball
  ( Ball (..),
    drawBall,
    drawHalo,
    updateBall,
  )
where

import Graphics.Gloss
import Vec (VectorSpace (..))

-- akin to Coulomb constant determines strength of force
chargeConstant :: Float
chargeConstant = 200000

timeStep :: Float
timeStep = 0.05

minDist :: Float
minDist = 20

-- Ball data type
data Ball v = Ball
  { pos :: v,
    vel :: v,
    charge :: Float,
    radius :: Float,
    col :: Color
  }
  deriving (Eq)

drawBall :: (VectorSpace v) => Ball v -> Picture
drawBall Ball {..} = Pictures [label, ball]
  where
    (x, y) = (vx pos, vy pos)
    offset = radius + 5
    showCharge c = (if c >= 0 then "+" else "") ++ take 5 (show c)
    ball = translate x y $ color col $ circleSolid radius
    label = translate x (offset + y) $ color white $ scale 0.1 0.1 $ Text (showCharge charge)

-- Draw halo whose size and opacity reflects force magnitude
drawHalo :: (VectorSpace v, Eq v) => Float -> Ball v -> [Ball v] -> Picture
drawHalo t b balls =
  let accForce = sumForceOn b balls
      mag = vmag accForce

      -- pulse directly dependant on force magnitude
      pulse = 0.3 + 0.5 * abs (sin (t * 4 + mag))

      -- Halo variable circle size and variable thickness pulses with force strength
      radiusPulse = radius b + 5 + 4 * pulse * (1 + min mag 0.01)
      thicknessPulse = 2 + 2 * pulse * min mag 20

      alpha = min 0.8 (pulse * (1 + mag / 5)) -- variable intensity
      haloColor = makeColor 1 1 1 alpha
      (x, y) = (vx $ pos b, vy $ pos b)
   in Translate x y $ Color haloColor $ ThickCircle (radiusPulse / 2) thicknessPulse

-- showCharge :: Float -> String

-- Update Physics
updateBall :: (VectorSpace v, Eq v) => Int -> Int -> [Ball v] -> Ball v -> Ball v
updateBall w h balls b =
  -- Sum all forces from other balls, assume mass = 1, acceleration = force / mass = force
  let accForce = sumForceOn b balls
      -- Update velocity and position
      newVel = vadd (vel b) $ vscale timeStep accForce
      newPos = vadd (pos b) $ vscale timeStep newVel

      -- Update position if ball is close enough to walls, simulating bouncing
      window = (vinit [fromIntegral w / 2, fromIntegral h / 2])
      (position, velocity) = checkWall newPos newVel window (radius b)
   in b {pos = position, vel = velocity}

checkWall :: (VectorSpace v) => v -> v -> v -> Float -> (v, v)
checkWall pos vel bound radius = (vinit pos', vinit vel')
  where
    (pos', vel') = unzip $ vmap3 wallAdjust pos vel bound
    wallAdjust :: Float -> Float -> Float -> (Float, Float)
    wallAdjust p v b
      -- if wall is hit, flip the velocity (given each coord dimension)
      -- position ball right at wall edge
      | p - radius < (-b) = (-b + radius, -v)
      | p + radius > b = (b - radius, -v)
      | otherwise = (p, v)

-- Sum force on one ball from all others
sumForceOn :: (VectorSpace v, Eq v) => Ball v -> [Ball v] -> v
sumForceOn cur balls = foldr vadd vzero [forceOn cur other | other <- balls, other /= cur]

forceOn :: (VectorSpace v) => Ball v -> Ball v -> v
forceOn Ball {pos = p1, charge = q1} Ball {pos = p2, charge = q2} =
  let r = vsub p1 p2 -- displacement vector
      dist = max minDist (vmag r)
      dir = vnorm r
      mag = chargeConstant * q1 * q2 / (dist * dist)
   in vscale mag dir -- Coulomb's law

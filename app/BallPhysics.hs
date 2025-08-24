{-# LANGUAGE RecordWildCards #-}

module BallPhysics
  ( Ball (..),
    initBalls,
    drawBall,
    updateBall,
  )
where

import Graphics.Gloss
import Vec (Vec2 (..), VectorSpace (..))

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

initBalls :: ((Float, Float) -> v) -> [Ball v]
initBalls vctor =
  [ Ball {pos = vctor (-150, 100), vel = vctor (40, -30), radius = 25, col = red, charge = 1},
    Ball {pos = vctor (120, -80), vel = vctor (-30, 50), radius = 18, col = blue, charge = -1},
    Ball {pos = vctor (-50, 60), vel = vctor (20, -20), radius = 22, col = green, charge = 1}
  ]

drawBall :: (VectorSpace v) => Ball v -> Picture
drawBall Ball {..} = ball
  where
    (x, y) = (vx pos, vy pos)
    --- translate: move picture by (x, y)
    ball = translate x y $ color col $ circleSolid radius

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
      dist = max minDist (vlen r)
      dir = vnorm r
      mag = chargeConstant * q1 * q2 / (dist * dist)
   in vscale mag dir -- Coulomb's law

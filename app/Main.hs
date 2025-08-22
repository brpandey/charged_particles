{-# LANGUAGE RecordWildCards #-}

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Vec (Vec2 (..), VectorSpace (..))

-- Ball data type
data Ball v = Ball
  { pos :: v,
    vel :: v,
    charge :: Float,
    radius :: Float,
    col :: Color
  }
  deriving (Eq)

data World v = World
  {balls :: [Ball v]}

-- Window constants
width, height :: Int
width = 800
height = 600

-- akin to Coulomb constant determines strength of force
chargeConstant :: Float
chargeConstant = 200000

timeStep :: Float
timeStep = 0.05

minDist :: Float
minDist = 20

-- Main
main :: IO ()
main = do
  let balls = initBalls Vec2
      world = World balls
  simulate display bgColor fps world render update
  where
    display = InWindow "Charged Patricles: Attract and Repel" (width, height) (100, 100)
    bgColor = black
    fps = 60

render :: (VectorSpace v, Eq v) => World v -> Picture
render w = drawBalls (balls w)

update :: (VectorSpace v, Eq v) => ViewPort -> Float -> World v -> World v
update _ _ w =
  w {balls = map (updateBall b) b}
  where
    b = balls w

initBalls :: ((Float, Float) -> v) -> [Ball v]
initBalls vctor =
  [ Ball {pos = vctor (-150, 100), vel = vctor (40, -30), radius = 25, col = red, charge = 1},
    Ball {pos = vctor (120, -80), vel = vctor (-30, 50), radius = 18, col = blue, charge = -1},
    Ball {pos = vctor (-50, 60), vel = vctor (20, -20), radius = 22, col = green, charge = 1}
  ]

drawBalls :: (VectorSpace v, Eq v) => [Ball v] -> Picture
drawBalls balls =
  Pictures [drawBall b | b <- balls]

drawBall :: (VectorSpace v) => Ball v -> Picture
drawBall Ball {..} = ball
  where
    (x, y) = (vx pos, vy pos)
    --- translate: move picture by (x, y)
    ball = translate x y $ color col $ circleSolid radius

-- Sum force on one ball from all others
sumForceOn :: (VectorSpace v, Eq v) => Ball v -> [Ball v] -> v
sumForceOn cur balls = foldr vadd vzero [forceOn cur other | other <- balls, other /= cur]

-- Update Physics
updateBall :: (VectorSpace v, Eq v) => [Ball v] -> Ball v -> Ball v
updateBall balls b =
  -- Sum all forces from other balls, assume mass = 1, acceleration = force / mass = force
  let accForce = sumForceOn b balls
      -- Update velocity and position
      newVel = vadd (vel b) $ vscale timeStep accForce
      newPos = vadd (pos b) $ vscale timeStep newVel
   in b {pos = newPos, vel = newVel}

forceOn :: (VectorSpace v) => Ball v -> Ball v -> v
forceOn Ball {pos = p1, charge = q1} Ball {pos = p2, charge = q2} =
  let r = vsub p1 p2 -- displacement vector
      dist = max minDist (vlen r)
      dir = vnorm r
      mag = chargeConstant * q1 * q2 / (dist * dist)
   in vscale mag dir -- Coulomb's law

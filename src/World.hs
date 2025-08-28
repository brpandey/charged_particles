module World
  ( World (..),
    setup,
    draw,
    render,
    update,
    width,
    height,
  )
where

import Ball
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import System.Random
import Vec (VectorSpace (..))

data World v = World
  { balls :: [Ball v],
    time :: Float
  }

-- Colors Gloss provides by default
glossColors :: [Color]
glossColors =
  [ red,
    green,
    blue,
    yellow,
    cyan,
    magenta,
    orange,
    rose,
    chartreuse,
    azure,
    violet
  ]

-- Window constants
width, height :: Int
width = 800
height = 600

render :: (VectorSpace v, Eq v, Monoid v) => World v -> Picture
render w = draw (time w) (balls w)

update :: (VectorSpace v, Eq v, Monoid v) => ViewPort -> Float -> World v -> World v
update _ t w =
  w
    { balls = map (updateBall width height b) b,
      time = time w + t
    }
  where
    b = balls w

draw :: (VectorSpace v, Eq v, Monoid v) => Float -> [Ball v] -> Picture
draw t list =
  Pictures [pair | b <- list, pair <- [drawHalo t b list, drawBall b]]

setup :: ((Float, Float) -> v) -> Int -> StdGen -> World v -- [Ball v]
setup vctor n gen =
  let -- Split the generator into four independent generators
      (genPos, gen1) = splitGen gen
      (genVel, gen2) = splitGen gen1
      (genCharge, gen3) = splitGen gen2

      -- Generate random values for positions, velocities, charges, and radii
      -- Cycle through built-in gloss colors
      positions = take (2 * n) $ randomRs (-1, 1) genPos
      velocities = take (2 * n) $ randomRs (-1, 1) genVel
      charges = take n $ randomRs (-1, 1) genCharge
      -- _ = trace ("Charges are " ++ show charges) ()

      radii = take n $ randomRs (8, 20) gen3
      colors = take n $ cycle glossColors
      -- pairs :: [a] -> [(a, a)]
      pairs (x : y : rest) = (x, y) : pairs rest
      pairs _ = []
      createBall (x, y) (velx, vely) (c, r, clr) =
        Ball
          { pos = vctor (x * fromIntegral width / 2, y * fromIntegral height / 2),
            vel = vctor (velx * 50, vely * 50),
            charge = c,
            radius = r,
            col = clr
          }
      ballList = zipWith3 createBall (pairs positions) (pairs velocities) (zip3 charges radii colors)
   in World ballList 0

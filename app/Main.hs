import Ball
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Vec (Vec2 (..), VectorSpace (..))

data World v = World
  { balls :: [Ball v],
    time :: Float
  }

-- Window constants
width, height :: Int
width = 800
height = 600

-- Main
main :: IO ()
main = do
  simulate window bgColor fps world render update
  where
    b = initBalls Vec2
    world = World b 0
    window = InWindow "Charged Patricles: Attract and Repel" (width, height) (100, 100)
    bgColor = black
    fps = 60

render :: (VectorSpace v, Eq v) => World v -> Picture
render w = draw (time w) (balls w)

update :: (VectorSpace v, Eq v) => ViewPort -> Float -> World v -> World v
update _ t w =
  w
    { balls = map (updateBall width height b) b,
      time = time w + t
    }
  where
    b = balls w

draw :: (VectorSpace v, Eq v) => Float -> [Ball v] -> Picture
draw t list =
  Pictures [pair | b <- list, pair <- [drawHalo t b list, drawBall b]]

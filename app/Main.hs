import BallPhysics
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Vec (Vec2 (..), VectorSpace (..))

data World v = World
  {balls :: [Ball v]}

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
    world = World b
    window = InWindow "Charged Patricles: Attract and Repel" (width, height) (100, 100)
    bgColor = black
    fps = 60

render :: (VectorSpace v, Eq v) => World v -> Picture
render w = draw (balls w)

update :: (VectorSpace v, Eq v) => ViewPort -> Float -> World v -> World v
update _ _ w =
  w {balls = map (updateBall width height b) b}
  where
    b = balls w

draw :: (VectorSpace v, Eq v) => [Ball v] -> Picture
draw list =
  Pictures [drawBall b | b <- list]

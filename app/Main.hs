import Ball ()
import Graphics.Gloss
import System.Random
import Vec (Vec2 (..))
import World as W

-- Main
main :: IO ()
main = do
  gen <- getStdGen
  let world = setup Vec2 8 gen
  simulate window bgColor fps world W.render W.update
  where
    window = InWindow "Charged Patricles: Attract and Repel" (width, height) (100, 100)
    bgColor = black
    fps = 60

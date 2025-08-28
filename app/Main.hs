import Ball ()
import Graphics.Gloss
import System.Environment (getArgs)
import System.Random
import Text.Read (readMaybe)
import Vec (Vec2 (..))
import World as W

maxBalls :: Int
maxBalls = 8

-- Main
main :: IO ()
main = do
  args <- getArgs
  count <- case args of
    [a] -> case readMaybe a of -- convert single arg into int
      Just n -> return (min n maxBalls)
      Nothing -> return maxBalls
    _ -> return maxBalls

  gen <- getStdGen
  let world = setup Vec2 count gen
  simulate window bgColor fps world W.render W.update
  where
    window = InWindow "Charged Patricles: Attract and Repel" (width, height) (100, 100)
    bgColor = black
    fps = 60

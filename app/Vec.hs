module Vec
  ( Vec2 (..),
    VectorSpace (..),
  )
where

newtype Vec2 = Vec2 (Float, Float)
  deriving (Eq, Show)

class VectorSpace v where
  vzero :: v
  vx :: v -> Float
  vy :: v -> Float
  vinit :: [Float] -> v -- create vec
  vadd :: v -> v -> v -- vector addition
  vsub :: v -> v -> v -- vector subtraction
  vscale :: Float -> v -> v -- scalar multiply
  vlen :: v -> Float -- magnitude
  vnorm :: v -> v -- normalized vector (unit length)
  vext :: Float -> v -- extend scalar
  vmap :: (Float -> a) -> v -> [a] -- maps over components
  vmap2 :: (Float -> Float -> a) -> v -> v -> [a]
  vmap3 :: (Float -> Float -> Float -> a) -> v -> v -> v -> [a]

instance VectorSpace Vec2 where
  vzero = Vec2 (0, 0)
  vx (Vec2 (x, _)) = x
  vy (Vec2 (_, y)) = y
  vinit [x, y] = Vec2 (x, y)
  vinit [x] = Vec2 (x, x)
  vinit _ = Vec2 (0, 0)
  vadd (Vec2 (x1, y1)) (Vec2 (x2, y2)) = Vec2 (x1 + x2, y1 + y2)
  vsub (Vec2 (x1, y1)) (Vec2 (x2, y2)) = Vec2 (x1 - x2, y1 - y2)
  vscale s (Vec2 (x, y)) = Vec2 (s * x, s * y)
  vlen (Vec2 (x, y)) = sqrt (x * x + y * y)
  vnorm v@(Vec2 (x, y)) =
    let m = vlen v
     in if m == 0 then Vec2 (0, 0) else Vec2 (x / m, y / m)
  vext s = Vec2 (s, s)
  vmap f (Vec2 (x, y)) = [f x, f y]
  vmap2 f (Vec2 (a, b)) (Vec2 (c, d)) = [f a c, f b d]
  vmap3 func (Vec2 (a, b)) (Vec2 (c, d)) (Vec2 (e, f)) = [func a c e, func b d f]

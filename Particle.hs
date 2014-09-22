{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Particle where
import Control.Applicative
import Control.Monad.Random
import Data.Typeable        (Typeable)
import FRP.Sodium

moveParticle :: Size -> Particle -> Behaviour Double -> Reactive (Behaviour Particle)
moveParticle area part timeB = do
  durs <- collect (\a b -> (a - b, a)) 0 timeB
  return $ updParticle area part <$> durs

data Particle = Particle { pos      :: (Double, Double)
                         , velocity :: (Double, Double)
                         } deriving (Read, Show, Eq, Ord, Typeable)

data Size = Size { width  :: Double
                 , height :: Double
                 } deriving (Read, Show, Eq, Ord, Typeable)

randomParticle :: (MonadRandom m, Applicative m) => Size -> m Particle
randomParticle Size{..} = do
  pos <- (,) <$> getRandomR (0, width) <*> getRandomR (0, height)
  vel <- (,) <$> getRandomR (-width*3 , width*3) <*> getRandomR (-height*3, height*3)
  return $ Particle pos vel

xcoord :: (a, b) -> a
xcoord = fst

ycoord :: (a, b) -> b
ycoord = snd

infixl 7 .*
(.*) :: Num t => t -> (t, t) -> (t, t)
a .* (b, c) = (a*b, a*c)

infixl 6 .+.
(.+.) :: (Num t1, Num t) => (t, t1) -> (t, t1) -> (t, t1)
(a, b) .+. (c, d) = (a + c, b + d)

inArea :: Size -> (Double, Double) -> Bool
inArea Size{..} (x, y) = 0 <= x && x <= width && 0 <= y && y <= height

updParticle :: Size -> Particle -> Double -> Particle
updParticle _ p 0 = p
updParticle area@Size{..} Particle{..} time =
  let nextPos = pos .+. time .* velocity
      colDur = min (calcCol width (fst velocity) (fst pos)) (calcCol height (snd velocity) (snd pos))
      interm = pos .+. colDur .* velocity
      newVel = flipVel interm
  in if inArea area nextPos
     then Particle nextPos velocity
     else updParticle area (Particle interm newVel) (time - colDur)
  where
    flipVel (xd, yd) =
      let (v, u) = velocity
          v' | 0 < xd && xd < width = v
             | otherwise = -v
          u' | 0 < yd && yd < height = u
             | otherwise = -u
      in (v', u')

calcCol :: (Ord a, Fractional a) => a -> a -> a -> a
calcCol ub v p
  | v < 0     = - p / v
  | v == 0    = 1 / 0
  | otherwise = (ub - p) / v

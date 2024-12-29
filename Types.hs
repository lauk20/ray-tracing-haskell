{-# LANGUAGE ExistentialQuantification #-}
module Types where
import Ray
import Color
import Vec3
import System.Random (RandomGen)

-- MaterialWrapper type
data MaterialWrapper = forall a. Material a => MaterialWrapper a

-- HitRecord type
data HitRecord = HitRecord { p :: Vec3, normal :: Vec3, t :: Double, frontFace :: Bool, mat :: MaterialWrapper}

-- Material type class
class Material a where
    scatter :: (RandomGen g) => a -> Ray -> HitRecord -> Color -> Ray -> g -> Maybe (Color, Ray, g)
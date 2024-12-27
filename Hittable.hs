{-# LANGUAGE ExistentialQuantification #-}
module Hittable where
import Ray
import Vector
import Vec3

-- HitRecord
data HitRecord = HitRecord {p :: Vec3, normal :: Vec3, t :: Double, frontFace :: Bool}

-- get the normal of a surface (outward normals convention)
-- returns the new HitRecord
getFaceNormal :: HitRecord -> Ray -> Vec3 -> HitRecord
getFaceNormal record r outwardNormal = HitRecord point newnormal (t record) frontFace
    where point = p record
          norm = normal record
          frontFace = (dot (direction r) outwardNormal) < 0
          newnormal = case frontFace of
            True -> outwardNormal
            False -> outwardNormal .* (-1)

-- Hittable objects typeclass
class Hittable a where
    hit :: a -> Ray -> Double -> Double -> Maybe HitRecord -> Maybe HitRecord

-- Wrapper that can hold any type that implements Hittable
data HittableObj = forall a . Hittable a => HittableObj a

-- HittableObj wrapper implementation
instance Hittable HittableObj where
    hit (HittableObj obj) r tMin tMax record = hit obj r tMin tMax record

-- Type for list of Hittable objects
data HittableList = HittableList {objects :: [HittableObj]}

-- HittableList implementation
instance Hittable HittableList where
    hit :: HittableList -> Ray -> Double -> Double -> Maybe HitRecord -> Maybe HitRecord
    hit list r rayTMin rayTMax hitRecord = hitHelper list rayTMax r rayTMin hitRecord

-- Helper function to loop through HittableList recursively
-- "Modifies" the HitRecord by returning a Maybe HitRecord
hitHelper :: HittableList -> Double -> Ray -> Double -> Maybe HitRecord -> Maybe HitRecord
hitHelper (HittableList []) closest r rayTMin hitRecord = hitRecord
hitHelper (HittableList (x:xs)) closest r rayTMin hitRecord =
    let newRecord = hit x r rayTMin closest hitRecord
    in case newRecord of
        Nothing -> hitHelper (HittableList xs) closest r rayTMin hitRecord
        Just record -> hitHelper (HittableList xs) (t record) r rayTMin (Just record)

module Sphere where
import Hittable
import Ray
import Vec3
import Vector

-- Sphere data type
data Sphere = Sphere {center :: Vec3, radius :: Double}

-- Sphere instance of hittable
instance Hittable Sphere where
    -- Hit function for sphere
    -- Returns a Maybe HitRecord
    hit :: Sphere -> Ray -> Double -> Double -> Maybe HitRecord -> Maybe HitRecord
    hit sphere ray rayTMin rayTMax hitRecord = 
        let 
            oc = ctr .- origin ray
            a = lengthSquared $ direction ray
            h = dot (direction ray) oc
            c = lengthSquared oc - r * r
            discriminant = h * h - a * c

            createHitRecord root =
                let
                    troot = root
                    pos = at ray troot
                    outwardNormal = (pos .- ctr) ./ r
                    newRecord = HitRecord pos outwardNormal troot False
                    finalRecord = getFaceNormal newRecord ray outwardNormal
                in case hitRecord of
                    Nothing -> Just finalRecord
                    Just prev -> if troot < t prev
                                 then Just finalRecord
                                 else hitRecord
            
            checkRoot root =
                if root <= rayTMin || rayTMax <= root
                then Nothing
                else createHitRecord root
            
            checkSecondRoot sqrtd a =
                let root2 = (h + sqrtd) / a
                in checkRoot root2
            
        in 
            if discriminant < 0
            then Nothing
            else
                let
                    sqrtd = sqrt discriminant
                    root = (h - sqrtd) / a
                in 
                    if root <= rayTMin || rayTMax <= root
                    then checkSecondRoot sqrtd a
                    else createHitRecord root

        where ctr = center sphere
              r = radius sphere
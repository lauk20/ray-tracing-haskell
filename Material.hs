module Material where
import Color
import Hittable
import Ray
import Types
import Utils
import Vector
import System.Random (RandomGen)

-- Lambertian (Diffuse) Materials
data Lambertian = Lambertian {albedo :: Color}

-- Implement Material type class
instance Material Lambertian where
    scatter :: (RandomGen g) => Lambertian -> Ray -> HitRecord -> Color -> Ray -> g -> Maybe (Color, Ray, g)
    scatter mat rIn hitRecord attenuation scattered gen =
        let 
            (randUnitVec, g1) = randomUnitVector gen
            scatterDirection = (normal hitRecord) .+ randUnitVec
            newScattered = Ray (p hitRecord) scatterDirection
            newAttenuation = albedo mat
        in case nearZero scatterDirection of
            True -> Just (normal hitRecord, newScattered, g1)
            False -> Just (newAttenuation, newScattered, g1)

-- Metal Materials
data Metal = Metal {albedo_metal :: Color}

-- Implement Material type class
instance Material Metal where
    scatter :: (RandomGen g) => Metal -> Ray -> HitRecord -> Color -> Ray -> g -> Maybe (Color, Ray, g)
    scatter mat rIn hitRecord attenuation scattered gen =
        let 
            reflected = reflectVector (direction rIn) (normal hitRecord)
            newScattered = Ray (p hitRecord) reflected
            newAttenuation = albedo_metal mat
        in Just (newAttenuation, newScattered, gen)
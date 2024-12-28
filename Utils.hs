module Utils where
import System.Random (random, mkStdGen, RandomGen)
import Vec3
import Vector

-- Definition for infinity
infinity :: Double
infinity = 1 / 0

-- Convert Degrees to Rads
degreesToRadians :: Double -> Double
degreesToRadians degrees = degrees * 3.1415925 / 180.0

-- Pure random generation using RandomGen
randomDouble :: (RandomGen g) => g -> (Double, g)
randomDouble gen = random gen

-- Get random number in range min max
randomDoubleMinMax :: (RandomGen g) => Double -> Double -> g -> (Double, g)
randomDoubleMinMax min max gen = 
    let (number, g') = randomDouble gen
    in (min + (max - min) * number, g')

-- Generate Random Vector
randomVec3 :: (RandomGen g) => g -> (Vec3, g)
randomVec3 gen =
    let
        (x, g1) = randomDouble gen
        (y, g2) = randomDouble g1
        (z, g3) = randomDouble g2
    in (MkVec3 x y z, g3)

-- Generate Random Vector in Range min max
randomVec3Range :: (RandomGen g) => Double -> Double -> g -> (Vec3, g)
randomVec3Range min max gen =
    let
        (x, g1) = randomDoubleMinMax min max gen
        (y, g2) = randomDoubleMinMax min max g1
        (z, g3) = randomDoubleMinMax min max g2
    in (MkVec3 x y z, g3)

-- Generate Random Unit Vector
randomUnitVector :: (RandomGen g) => g -> (Vec3, g)
randomUnitVector gen =
    let 
        (p, g1) = randomVec3Range (-1) 1 gen
        lensq = lengthSquared p
    in
        if 1e-160 < lensq && lensq <= 1
        then (p ./ sqrt lensq, g1)
        else randomUnitVector g1

-- Generate random vector on unit sphere
randomOnHemisphere :: (RandomGen g) => Vec3 -> g -> (Vec3, g)
randomOnHemisphere normal gen =
    let (onUnitSphere, g1) = randomUnitVector gen
    in case dot onUnitSphere normal > 0.0 of
        True -> (onUnitSphere, g1)
        False -> (onUnitSphere .* (-1), g1)
module Utils where
import System.Random (random, mkStdGen, RandomGen)

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
randomDoubleMinMax :: (RandomGen g) => Double -> Double -> g -> Double
randomDoubleMinMax min max gen = 
    let (number, g') = randomDouble gen
    in min + (max - min) * number
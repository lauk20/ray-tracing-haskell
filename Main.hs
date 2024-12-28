module Main where
import Color
import Data.List (foldl')
import Hittable
import PlainString
import Ray
import Sphere
import Utils
import Vector
import Vec3
import System.Random (random, mkStdGen, RandomGen, StdGen)

-- Aspect Ratio
aspectRatio :: Double
aspectRatio = 16 / 9

-- Dimensions of Image
imageWidth :: Int
imageWidth = 400
imageHeight :: Int
imageHeight = floor $ fromIntegral imageWidth / aspectRatio

-- Samples Per Pixel
samplesPerPixel :: Int
samplesPerPixel = 100
pixelSamplesScale :: Double
pixelSamplesScale = 1.0 / fromIntegral samplesPerPixel

-- Max Depth
maxDepth :: Int
maxDepth = 10

-- Camera
focalLength :: Double
focalLength = 1.0
viewportHeight :: Double
viewportHeight = 2.0
viewportWidth :: Double
viewportWidth = viewportHeight * (fromIntegral imageWidth / fromIntegral imageHeight)
cameraCenter :: Vec3
cameraCenter = MkVec3 0 0 0

-- Vectors Across Horizontal and Vertical Viewport Edges
viewportU :: Vec3
viewportU = MkVec3 viewportWidth 0 0
viewportV :: Vec3
viewportV = MkVec3 0 (viewportHeight * (-1)) 0

-- Viewport deltas between pixels
pixelDeltaU :: Vec3
pixelDeltaU = viewportU ./ fromIntegral imageWidth
pixelDeltaV :: Vec3
pixelDeltaV = viewportV ./ fromIntegral imageHeight

-- Location of upper left pixel
viewportUpperLeft :: Vec3
viewportUpperLeft = cameraCenter .- (MkVec3 0 0 focalLength) .- (viewportU ./ 2) .- (viewportV ./ 2)
pixel00Loc :: Vec3
pixel00Loc = viewportUpperLeft .+ ((pixelDeltaU .+ pixelDeltaV) .* 0.5)

-- Random Generator
gen :: StdGen
gen = mkStdGen 42

-- No Longer Being Used
-- -- Determine whether a sphere is hit
-- hitSphere :: Vec3 -> Double -> Ray -> Double
-- hitSphere center radius r = 
--     let oc = center .- origin r
--         a = lengthSquared $ direction r
--         h = dot (direction r) oc
--         c = lengthSquared oc - radius * radius
--         discriminant = h * h - a * c
--     in case discriminant < 0 of
--         True -> -1.0
--         _ -> (h - sqrt discriminant) / a

-- World of objects
world = HittableList [
            HittableObj (Sphere (MkVec3 0 0 (-1)) 0.5),
            HittableObj (Sphere (MkVec3 0 (-100.5) (-1)) 100)
        ]

-- Get Color of ray sent into scene
rayColor :: (RandomGen g) => Ray -> HittableList -> g -> Int -> (Color, g)
rayColor ray world gen iters =
    if iters == 0
    then (MkVec3 0 0 0, gen)
    else
        case hitSomething of
            Just hitRecord -> let (direction, g1) = randomOnHemisphere (normal hitRecord) gen
                            in (fst (rayColor (Ray (p hitRecord) (direction .+ (normal hitRecord))) world g1 (iters - 1)) .* 0.5, g1)
            Nothing -> ((MkVec3 1.0 1.0 1.0 .* (1.0 - a)) .+ (MkVec3 0.5 0.7 1.0 .* a), gen)
    where 
        unitDirection = unitVector $ direction ray
        a = (y unitDirection + 1.0) * 0.5
        hitSomething = hit world ray 0.001 (1 / 0) Nothing

-- Construct a camera ray originating from the origin and directed at randomly sampled
-- point around the pixel location i, j.
getRay :: (RandomGen g) => Int -> Int -> g -> (Ray, g)
getRay i j gen =
    let 
        (offset, gen1) = sampleSquare gen
        pixelSample = pixel00Loc
                .+ (pixelDeltaU .* (fromIntegral i + x offset))
                .+ (pixelDeltaV .* (fromIntegral j + y offset))
        rayOrigin = cameraCenter
        rayDirection = pixelSample .- rayOrigin
    in (Ray rayOrigin rayDirection, gen1)

-- Returns the vector to a random point in the [-.5,-.5]-[+.5,+.5] unit square.
sampleSquare :: (RandomGen g) => g -> (Vec3, g)
sampleSquare gen =
    let (randX, gen1) = randomDouble gen
        (randY, gen2) = randomDouble gen1
    in (MkVec3 (randX - 0.5) (randY - 0.5) 0, gen2)

-- Generate the RGB raster
-- Proper propagation of the random state
generateRGB :: (RandomGen g) => g -> [[Vec3]]
generateRGB gen =
    let (pixels, _) = foldl' generateRow ([], gen) [0..imageHeight - 1]
    in reverse pixels
    where
        generateRow :: (RandomGen g) => ([[Vec3]], g) -> Int -> ([[Vec3]], g)
        generateRow (acc, g) j =
            let (row, g') = foldl' (generatePixel j) ([], g) [0..imageWidth - 1]
            in (reverse row : acc, g')
        
        generatePixel :: (RandomGen g) => Int -> ([Vec3], g) -> Int -> ([Vec3], g)
        generatePixel j (acc, g) i =
            let (color, g') = getMultipleSamples i j g
            in (color .* pixelSamplesScale : acc, g')
        
        getMultipleSamples :: (RandomGen g) => Int -> Int -> g -> (Color, g)
        getMultipleSamples i j g =
            foldl' accumSamples (MkVec3 0 0 0, g) [1..samplesPerPixel]
            where
                accumSamples :: (RandomGen g) => (Color, g) -> Int -> (Color, g)
                accumSamples (accColor, gen) _ =
                    let 
                        (ray, gen1) = getRay i j gen
                        (sampleColor, gen2) = rayColor ray world gen1 maxDepth
                    in (accColor .+ sampleColor, gen2)
 
-- Main
main :: IO ()
main = do
    -- Render
    -- PPM Header
    print $ PlainString ("P3\n" ++ show imageWidth ++ " " ++ show imageHeight ++ "\n255")

    -- Print RGB Raster
    mapM_ (mapM_ writeColor) $ generateRGB gen
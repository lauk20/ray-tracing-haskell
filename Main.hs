module Main where
import Color
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
rayColor :: Ray -> HittableList -> Color
rayColor ray world =
    case hitSomething of
        Just hitRecord -> normal hitRecord .+ MkVec3 1 1 1 .* 0.5
        Nothing -> (MkVec3 1.0 1.0 1.0 .* (1.0 - a)) .+ (MkVec3 0.5 0.7 1.0 .* a)
    where 
        unitDirection = unitVector $ direction ray
        a = (y unitDirection + 1.0) * 0.5
        hitSomething = hit world ray 0 (1 / 0) Nothing

-- Construct a camera ray originating from the origin and directed at randomly sampled
-- point around the pixel location i, j.
getRay :: Int -> Int -> StdGen -> (Ray, StdGen)
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
sampleSquare :: StdGen -> (Vec3, StdGen)
sampleSquare gen =
    let (randX, gen1) = randomDouble gen
        (randY, gen2) = randomDouble gen1
    in (MkVec3 (randX - 0.5) (randY - 0.5) 0, gen2)

-- Generate the RGB raster
generateRGB :: [[Vec3]]
generateRGB = 
    [ [ (getMultipleSamples i j (MkVec3 0 0 0) gen) .* pixelSamplesScale
        | i <- [0..imageWidth - 1] ]
        | j <- [0..imageHeight - 1] ]
    where
        getMultipleSamples :: Int -> Int -> Color -> StdGen -> Color
        getMultipleSamples i j pixelColor gen = helper i j pixelColor samplesPerPixel gen
            where 
                helper :: Int -> Int -> Color -> Int -> StdGen -> Color
                helper i j pixelColor 0 gen = pixelColor
                helper i j pixelColor iterations gen = helper i j (pixelColor .+ rayColor r world) (iterations - 1) gen'
                    where (r, gen') = getRay i j gen
            
-- Main
main :: IO ()
main = do
    -- Render
    -- PPM Header
    print $ PlainString ("P3\n" ++ show imageWidth ++ " " ++ show imageHeight ++ "\n255")

    -- Print RGB Raster
    mapM_ (mapM_ writeColor) generateRGB
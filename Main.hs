module Main where
import Color
import Hittable
import PlainString
import Ray
import Sphere
import Vector
import Vec3

-- Aspect Ratio
aspectRatio :: Double
aspectRatio = 16 / 9

-- Dimensions of Image
imageWidth :: Int
imageWidth = 400
imageHeight :: Int
imageHeight = floor $ fromIntegral imageWidth / aspectRatio

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

-- Determine whether a sphere is hit
hitSphere :: Vec3 -> Double -> Ray -> Double
hitSphere center radius r = 
    let oc = center .- origin r
        a = lengthSquared $ direction r
        h = dot (direction r) oc
        c = lengthSquared oc - radius * radius
        discriminant = h * h - a * c
    in case discriminant < 0 of
        True -> -1.0
        _ -> (h - sqrt discriminant) / a

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

-- Generate the RGB raster
generateRGB :: [[Vec3]]
generateRGB = 
    [ [ let pixelCenter = pixel00Loc .+ (pixelDeltaU .* fromIntegral i) .+ (pixelDeltaV .* fromIntegral j)
            rayDirection = pixelCenter .- cameraCenter
            r = Ray cameraCenter rayDirection
        in rayColor r world
    | i <- [0..imageWidth - 1] ]
    | j <- [0..imageHeight - 1] ]

-- Main
main :: IO ()
main = do
    -- Render
    -- PPM Header
    print $ PlainString ("P3\n" ++ show imageWidth ++ " " ++ show imageHeight ++ "\n255")

    -- Print RGB Raster
    mapM_ (mapM_ writeColor) generateRGB
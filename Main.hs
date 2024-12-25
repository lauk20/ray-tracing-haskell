module Main where
import Color
import PlainString
import Ray
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

-- Get Color of ray sent into scene
rayColor :: Ray -> Color
rayColor ray = (MkVec3 1.0 1.0 1.0 .* (1.0 - a)) .+ (MkVec3 0.5 0.7 1.0 .* a)
    where unitDirection = unitVector $ direction ray
          a = (y unitDirection + 1.0) * 0.5

-- Generate the RGB raster
generateRGB :: [[Vec3]]
generateRGB = 
    [ [ let pixelCenter = pixel00Loc .+ (pixelDeltaU .* fromIntegral i) .+ (pixelDeltaV .* fromIntegral j)
            rayDirection = pixelCenter .- cameraCenter
            r = Ray cameraCenter rayDirection
        in rayColor r
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
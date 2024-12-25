module Main where
import Color
import PlainString
import Vec3

-- Dimensions of Image
imageHeight :: Int
imageHeight = 256
imageWidth :: Int
imageWidth = 256

-- Generate the RGB raster
generateRGB :: [[Vec3]]
generateRGB = 
    [[
        MkVec3 (fromIntegral i / fromIntegral (imageWidth - 1)) (fromIntegral j / fromIntegral (imageHeight - 1)) 0
    | i <- [0..imageWidth - 1]]
    | j <- [0..imageHeight - 1]]

-- Main
main :: IO ()
main = do
    -- Render
    -- PPM Header
    print $ PlainString ("P3\n" ++ show imageWidth ++ " " ++ show imageHeight ++ "\n255")
    
    -- Print RGB Raster
    mapM_ (mapM_ writeColor) generateRGB
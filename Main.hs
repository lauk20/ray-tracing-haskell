module Main where

-- Define Strings without Quotes for Printing
newtype PlainString = PlainString String

instance Show PlainString where
    show :: PlainString -> String
    show (PlainString str) = str

-- Dimensions of Image
imageHeight :: Int
imageHeight = 256
imageWidth :: Int
imageWidth = 256

-- Generate the RGB raster
generateRGB :: [[(Int, Int, Int)]]
generateRGB = 
    [[
        (floor ( 255.999 * (fromIntegral i / fromIntegral (imageWidth - 1)) ),
         floor ( 255.999 * (fromIntegral j / fromIntegral (imageHeight - 1)) ),
         0)
    | i <- [0..imageWidth - 1]]
    | j <- [0..imageHeight - 1]]

-- Main
main :: IO ()
main = do
    -- Render
    -- PPM Header
    print $ PlainString ("P3\n" ++ show imageWidth ++ " " ++ show imageHeight ++ "\n255")
    
    -- RGB Raster
    mapM_ (mapM_ (\(r, g, b) -> print $ PlainString (show r ++ " " ++ show g ++ " " ++ show b))) generateRGB
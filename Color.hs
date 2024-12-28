module Color where
import Interval
import PlainString
import Vec3

type Color = Vec3

-- Convert color to gamma scale
linearToGamma :: Double -> Double
linearToGamma linearComponent
    | linearComponent > 0 = sqrt linearComponent
    | otherwise = 0

-- print out the RGB values
writeColor :: Color -> IO ()
writeColor (MkVec3 r g b) = do
    print $ PlainString (show rbyte ++ " " ++ show gbyte ++ " " ++ show bbyte)
    where 
        intensity = Interval 0.0 0.999
        rbyte = floor $ 255.999 * clamp intensity (linearToGamma r)
        gbyte = floor $ 255.999 * clamp intensity (linearToGamma g)
        bbyte = floor $ 255.999 * clamp intensity (linearToGamma b)
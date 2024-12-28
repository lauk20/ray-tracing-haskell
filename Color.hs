module Color where
import Interval
import PlainString
import Vec3

type Color = Vec3

-- print out the RGB values
writeColor :: Color -> IO ()
writeColor (MkVec3 r g b) = do
    print $ PlainString (show rbyte ++ " " ++ show gbyte ++ " " ++ show bbyte)
    where 
        intensity = Interval 0.0 0.999
        rbyte = floor $ 255.999 * clamp intensity r
        gbyte = floor $ 255.999 * clamp intensity g
        bbyte = floor $ 255.999 * clamp intensity b
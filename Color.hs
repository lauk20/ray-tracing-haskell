module Color where
import PlainString
import Vec3

type Color = Vec3

-- print out the RGB values
writeColor :: Color -> IO ()
writeColor (MkVec3 r g b) = do
    print $ PlainString (show rbyte ++ " " ++ show gbyte ++ " " ++ show bbyte)
    where rbyte = floor $ 255.999 * r
          gbyte = floor $ 255.999 * g
          bbyte = floor $ 255.999 * b
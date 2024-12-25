module Vec3 where
import Vector

-- Vec3 data type
data Vec3 = MkVec3 {x :: Double, y :: Double, z :: Double}
    deriving Show

-- Vec3 instance of Vec
instance Vector Vec3 where
    -- Vector Addition
    (.+) :: Vec3 -> Vec3 -> Vec3
    (MkVec3 x1 y1 z1) .+ (MkVec3 x2 y2 z2) = MkVec3 (x1 + x2) (y1 + y2) (z1 + z2)

    -- Vector Subtraction
    (.-) :: Vec3 -> Vec3 -> Vec3
    (MkVec3 x1 y1 z1) .- (MkVec3 x2 y2 z2) = MkVec3 (x1 - x2) (y1 - y2) (z1 - z2)

    -- Multiply Element-wise
    (.*) :: Vec3 -> Double-> Vec3
    (MkVec3 x1 y1 z1) .* num = MkVec3 (x1 * num) (y1 * num) (z1 * num)

    -- Divide Element-wise
    (./) :: Vec3 -> Double -> Vec3
    (MkVec3 x1 y1 z1) ./ num = MkVec3 (x1 / num) (y1 / num) (z1 / num)

    -- Dot Product
    dot :: Vec3 -> Vec3 -> Double
    dot (MkVec3 x1 y1 z1) (MkVec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

    -- Cross Product
    cross :: Vec3 -> Vec3 -> Vec3
    cross (MkVec3 u0 u1 u2) (MkVec3 v0 v1 v2) = MkVec3 (u1 * v2 - u2 * v1) (u2 * v0 - u0 * v2) (u0 * v1 - u1 * v0)

    -- Length Squared
    lengthSquared :: Vec3 -> Double
    lengthSquared (MkVec3 u0 u1 u2) = u0 * u0 + u1 * u1 + u2 * u2

    -- Length
    length :: Vec3 -> Double
    length = sqrt . lengthSquared

    -- Get Unit Vector
    unitVector :: Vec3 -> Vec3
    unitVector vec = vec ./ Vector.length vec
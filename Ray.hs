module Ray where
import Vector
import Vec3

-- Ray data structure
data Ray = Ray {origin :: Vec3, direction :: Vec3}

-- Get Position of Ray at parameter t
at :: Ray -> Double -> Vec3
at ray t = origin ray .+ (direction ray .* t)
    
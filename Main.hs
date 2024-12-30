module Main where
import Color
import Data.List (foldl')
import Hittable
import Material
import PlainString
import Ray
import Sphere
import Types
import Utils
import Vector
import Vec3
import System.Random (random, mkStdGen, RandomGen, StdGen)
import Prelude hiding (length)

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
samplesPerPixel = 50
pixelSamplesScale :: Double
pixelSamplesScale = 1.0 / fromIntegral samplesPerPixel

-- Max Depth
maxDepth :: Int
maxDepth = 10

-- FOV
vfov :: Double
vfov = 20

-- Position and Direction of Camera
lookFrom :: Vec3
lookAt :: Vec3
vup :: Vec3
lookFrom = MkVec3 13 2 3
lookAt = MkVec3 0 0 0
vup = MkVec3 0 1 0

-- Camera
focalLength :: Double
theta :: Double
h :: Double
viewportHeight :: Double
viewportWidth :: Double
cameraCenter :: Vec3
focalLength = length (lookFrom .- lookAt)
theta = degreesToRadians vfov
h = tan $ theta / 2
viewportHeight = 2.0 * h * focalLength
viewportWidth = viewportHeight * (fromIntegral imageWidth / fromIntegral imageHeight)
cameraCenter = lookFrom

-- Basis Vectors
u :: Vec3
v :: Vec3
w :: Vec3
w = unitVector (lookFrom .- lookAt)
u = unitVector $ cross vup w
v = cross w u

-- Vectors Across Horizontal and Vertical Viewport Edges
viewportU :: Vec3
viewportU = u .* viewportWidth
viewportV :: Vec3
viewportV = v .* ((-1) * viewportHeight)

-- Viewport deltas between pixels
pixelDeltaU :: Vec3
pixelDeltaU = viewportU ./ fromIntegral imageWidth
pixelDeltaV :: Vec3
pixelDeltaV = viewportV ./ fromIntegral imageHeight

-- Location of upper left pixel
viewportUpperLeft :: Vec3
viewportUpperLeft = cameraCenter .- (w .* focalLength) .- (viewportU ./ 2) .- (viewportV ./ 2)
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

-- Materials
materialGround :: MaterialWrapper
materialCenter :: MaterialWrapper
materialLeft :: MaterialWrapper
materialRight :: MaterialWrapper
materialGround = MaterialWrapper (Lambertian (MkVec3 0.5 0.5 0.5))
materialCenter = MaterialWrapper (Lambertian (MkVec3 0.1 0.2 0.5))
materialLeft = MaterialWrapper (Metal (MkVec3 0.8 0.8 0.8))
materialRight = MaterialWrapper (Metal (MkVec3 0.8 0.6 0.2))

-- World of objects
-- world = HittableList [
--             HittableObj (Sphere (MkVec3 0 (-100.5) (-1)) 100 materialGround),
--             HittableObj (Sphere (MkVec3 0 0 (-1.2)) 0.5 materialCenter),
--             HittableObj (Sphere (MkVec3 (-1.0) 0 (-1)) 0.5 materialLeft),
--             HittableObj (Sphere (MkVec3 1.0 0 (-1)) 0.5 materialRight)
--         ]

-- Create random world
createWorld :: (RandomGen g) => g -> HittableList -> (HittableList, g)
createWorld gen list =
    let
        createDiffuse :: (RandomGen g) => (HittableList, g) -> Vec3 -> (HittableList, g)
        createDiffuse (curr, gen) center =
            let
                (rand1, g1) = randomVec3 gen
                (rand2, g2) = randomVec3 g1
                albedo = rand1 */* rand2
                sphereMaterial = MaterialWrapper (Lambertian albedo)
                sphere = HittableObj (Sphere center 0.2 sphereMaterial)
            in (HittableList (sphere : (objects curr)), g2)
        
        createMetal :: (RandomGen g) => (HittableList, g) -> Vec3 -> (HittableList, g)
        createMetal (curr, g) center =
            let
                (rand1, g1) = randomVec3Range 0 0.5 g
                albedo = rand1
                sphereMaterial = MaterialWrapper (Metal albedo)
                sphere = HittableObj (Sphere center 0.2 sphereMaterial)
            in (HittableList (sphere : (objects curr)), g1)
        
        createObj :: (RandomGen g) => Int -> Int -> (HittableList, g) -> (HittableList, g)
        createObj a b (curr, gen) =
            let
                (chooseMat, g1) = randomDouble gen
                (rand1, g2) = randomDouble g1
                (rand2, g3) = randomDouble g2
                center = MkVec3 (fromIntegral a + 0.9 * rand1) 0.2 (fromIntegral b + 0.9 * rand2)
            in case sqrt (lengthSquared (center .- MkVec3 4 0.2 0)) > 0.9 of
                True -> case chooseMat of
                            chooseMat | chooseMat < 0.8 -> createDiffuse (curr, g3) center
                            chooseMat | chooseMat < 1 -> createMetal (curr, g3) center
                False -> (curr, g3)
    in foldl (\(world, g) a -> foldl (\(world', g') b -> createObj a b (world', g')) (world, g) [-11..10]) (list, gen) [-11..10]

-- Get Color of ray sent into scene
rayColor :: (RandomGen g) => Ray -> HittableList -> g -> Int -> (Color, g)
rayColor ray world gen iters =
    if iters == 0
    then (MkVec3 0 0 0, gen)
    else
        case hitSomething of
            Just hitRecord -> 
                case mat hitRecord of
                    MaterialWrapper mr ->
                            let scattered = Ray (MkVec3 0 0 0) (MkVec3 0 0 0)
                                attenuation = MkVec3 0 0 0
                                sc = scatter mr ray hitRecord attenuation scattered gen
                            in case sc of
                                Just (newColor, newScattered, g1) -> 
                                    let (clr, g2) = rayColor newScattered world g1 (iters - 1)
                                    in (newColor */* clr, g2)
                                Nothing -> (MkVec3 0 0 0, gen)
            Nothing -> ((MkVec3 1.0 1.0 1.0 .* (1.0 - a)) .+ (MkVec3 0.5 0.7 1.0 .* a), gen)
    where 
        unitDirection = unitVector $ direction ray
        a = (y unitDirection + 1.0) * 0.5
        hitSomething = hit world ray 0.001 (1 / 0) Nothing

-- Construct a camera ray originating from the origin and directed at randomly sampled
-- point around the pixel location i, j.
getRay :: (RandomGen g) => Int -> Int -> g -> (Ray, g)
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
sampleSquare :: (RandomGen g) => g -> (Vec3, g)
sampleSquare gen =
    let (randX, gen1) = randomDouble gen
        (randY, gen2) = randomDouble gen1
    in (MkVec3 (randX - 0.5) (randY - 0.5) 0, gen2)

-- Generate the RGB raster
-- Proper propagation of the random state
generateRGB :: (RandomGen g) => g -> [[Vec3]]
generateRGB gen =
    let (pixels, _) = foldl' generateRow ([], gen) [0..imageHeight - 1]
    in reverse pixels
    where
        generateRow :: (RandomGen g) => ([[Vec3]], g) -> Int -> ([[Vec3]], g)
        generateRow (acc, g) j =
            let (row, g') = foldl' (generatePixel j) ([], g) [0..imageWidth - 1]
            in (reverse row : acc, g')
        
        generatePixel :: (RandomGen g) => Int -> ([Vec3], g) -> Int -> ([Vec3], g)
        generatePixel j (acc, g) i =
            let (color, g') = getMultipleSamples i j g
            in (color .* pixelSamplesScale : acc, g')
        
        getMultipleSamples :: (RandomGen g) => Int -> Int -> g -> (Color, g)
        getMultipleSamples i j g =
            foldl' accumSamples (MkVec3 0 0 0, g) [1..samplesPerPixel]
            where
                accumSamples :: (RandomGen g) => (Color, g) -> Int -> (Color, g)
                accumSamples (accColor, gen) _ =
                    let 
                        (ray, gen1) = getRay i j gen
                        (sampleColor, gen2) = rayColor ray randomWorld gen1 maxDepth
                    in (accColor .+ sampleColor, gen2)

-- Create Our World
(worldList, ngen) = createWorld gen (HittableList [])

material2 :: MaterialWrapper
material3 :: MaterialWrapper
material2 = MaterialWrapper (Lambertian (MkVec3 0.4 0.2 0.1))
material3 = MaterialWrapper (Metal (MkVec3 0.7 0.6 0.5))

mainObjs :: [HittableObj]
mainObjs = [
        HittableObj (Sphere (MkVec3 0 (-1000) 0) 1000 materialGround),
        HittableObj (Sphere (MkVec3 (-4) 1 0) 1 material2),
        HittableObj (Sphere (MkVec3 4 1 0) 1 material3)
    ]
randomWorld = HittableList $ mainObjs ++ (objects worldList)

-- Main
main :: IO ()
main = do
    -- Render
    -- PPM Header
    print $ PlainString ("P3\n" ++ show imageWidth ++ " " ++ show imageHeight ++ "\n255")

    -- Print RGB Raster
    mapM_ (mapM_ writeColor) $ generateRGB gen
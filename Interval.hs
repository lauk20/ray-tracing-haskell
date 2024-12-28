module Interval where
import Prelude hiding (max, min)
import Utils

-- Interval type
data Interval = Interval {min :: Double, max :: Double}

-- Get size of interval
intervalSize :: Interval -> Double
intervalSize interval = max interval - min interval

-- Whether the interval contains a value
intervalContains :: Interval -> Double -> Bool
intervalContains interval x = min interval <= x && x <= max interval

-- Whether the interval surrounds a value
intervalSurrounds :: Interval -> Double -> Bool
intervalSurrounds interval x = min interval < x && x < max interval

-- The empty interval
emptyInterval :: Interval
emptyInterval = Interval infinity (-infinity)

-- The universal interval
universeInterval :: Interval
universeInterval = Interval (-infinity) infinity

-- Clamp a value to interval
clamp :: Interval -> Double -> Double
clamp (Interval min max) x
    | x < min = min
    | x > max = max
    | otherwise = x
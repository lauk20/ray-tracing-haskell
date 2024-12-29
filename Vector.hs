module Vector where

-- Vector typeclass
class Vector a where
    (.+) :: a -> a -> a
    (.-) :: a -> a -> a
    (.*) :: a -> Double -> a
    (./) :: a -> Double -> a
    (*/*) :: a -> a -> a
    dot :: a -> a -> Double
    cross :: a -> a -> a
    lengthSquared :: a -> Double
    length :: a -> Double
    unitVector :: a -> a
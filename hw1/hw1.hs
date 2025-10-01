module HomeWork1 where

-- CSCE 314 • Homework 1: Geometry Studio (Starter Stub)
-- Name: Aadi Joshi
-- UIN: 935007479
-- I worked independently, but I may have discussed general ideas with:

-- You may import NOTHING.

-- ===== 1) Core Shape model (ADT) =====
-- TODO: You will implement functions over this ADT using pattern matching and guards.
data Shape
  = Circle Double           -- radius
  | Rectangle Double Double -- width height
  | Square Double           -- side
  deriving (Eq, Show)

-- ===== 2) A recursive list for Shapes (recursive ADT) =====
-- TODO: You'll write list-like functions over this custom list.
data ShapeList
  = SEmpty
  | SCons Shape ShapeList
  deriving (Eq, Show)

-- ===== 3) Required functions =====
-- PART A: Single-shape utilities (pattern matching + guards)

-- area: Circle r -> pi*r^2; Rectangle w h -> w*h; Square s -> s*s
area :: Shape -> Double
area (Circle r)     = pi * r * r
area (Rectangle w h) = w * h
area (Square s)     = s * s

-- perimeter: Circle r -> 2*pi*r; Rectangle w h -> 2*(w+h); Square s -> 4*s
perimeter :: Shape -> Double
perimeter (Circle r)     = 2 * pi * r
perimeter (Rectangle w h) = 2 * (w + h)
perimeter (Square s)     = 4 * s

-- scale k shape: for k>0 scale dimensions by k; otherwise return shape unchanged (use a guard).
scale :: Double -> Shape -> Shape
scale k sh
  | k > 0 = case sh of
              Circle r       -> Circle (k * r)
              Rectangle w h  -> Rectangle (k * w) (k * h)
              Square s       -> Square (k * s)
  | otherwise = sh

-- Identify shape “kind” exactly as these strings: "Circle" | "Rectangle" | "Square"
kind :: Shape -> String
kind (Circle _)     = "Circle"
kind (Rectangle _ _) = "Rectangle"
kind (Square _)     = "Square"

-- PART B: Working with lists of shapes (recursion + higher-order functions)

-- Sum of areas of all shapes in a list
totalArea :: [Shape] -> Double
totalArea shapes = sum (map area shapes)

-- Sum of perimeters of all shapes in a list
totalPerimeter :: [Shape] -> Double
totalPerimeter shapes = sum (map perimeter shapes)

-- Count how many of each kind are present; return (circles, rectangles, squares)
countByKind :: [Shape] -> (Int, Int, Int)
countByKind shapes = foldr step (0,0,0) shapes
  where
    step (Circle _) (c,r,s)      = (c+1, r, s)
    step (Rectangle _ _) (c,r,s) = (c, r+1, s)
    step (Square _) (c,r,s)      = (c, r, s+1)

-- Largest area in the list; Nothing for []
-- If areas tie, keep the FIRST (i.e., only replace when strictly greater).
maxByArea :: [Shape] -> Maybe Shape
maxByArea []     = Nothing
maxByArea (x:xs) = Just (helper x xs)
  where
    helper best [] = best
    helper best (y:ys)
      | area y > area best = helper y ys
      | otherwise          = helper best ys

-- Scale all shapes by k using your `scale` function
scaleAll :: Double -> [Shape] -> [Shape]
scaleAll k shapes = map (scale k) shapes

-- Keep only shapes with area >= t
filterByMinArea :: Double -> [Shape] -> [Shape]
filterByMinArea t shapes = filter (\s -> area s >= t) shapes

-- PART C: The recursive ShapeList ADT (structural recursion over SEmpty/SCons)

-- Convert a Haskell list [Shape] to ShapeList
toShapeList :: [Shape] -> ShapeList
toShapeList []     = SEmpty
toShapeList (x:xs) = SCons x (toShapeList xs)

-- Convert ShapeList back to a Haskell list
fromShapeList :: ShapeList -> [Shape]
fromShapeList SEmpty       = []
fromShapeList (SCons x xs) = x : fromShapeList xs

-- Sum of areas over ShapeList
sumAreasSL :: ShapeList -> Double
sumAreasSL SEmpty       = 0
sumAreasSL (SCons x xs) = area x + sumAreasSL xs

-- Length of ShapeList
lengthSL :: ShapeList -> Int
lengthSL SEmpty       = 0
lengthSL (SCons _ xs) = 1 + lengthSL xs

-- Map-like operation over ShapeList using a function on Shape
mapSL :: (Shape -> Shape) -> ShapeList -> ShapeList
mapSL f SEmpty       = SEmpty
mapSL f (SCons x xs) = SCons (f x) (mapSL f xs)

-- ===== 4) A small built-in studio “scene” for your transcript tests =====
-- You can use this for quick local checks in ghci.
scene :: [Shape]
scene =
  [Circle 3,
   Rectangle 2 5,
   Square 4,
   Circle 1,
   Rectangle 3 3
  ]


{- ===========================================================
aadijoshi@Aadis-MacBook-Air hw1 % ghci hw1.hs
GHCi, version 9.6.7: https://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling HomeWork1        ( hw1.hs, interpreted )
Ok, one module loaded.
ghci> area (Circle 3)
28.274333882308138
ghci> perimeter (Rectangle 2 5)
14.0
ghci> kind (Square 4)
"Square"
ghci> scale 2 (Rectangle 1 2)
Rectangle 2.0 4.0
ghci> scale (-1) (Circle 10)
Circle 10.0
ghci> totalArea scene
66.41592653589794
ghci> totalPerimeter scene
67.13274122871834
ghci> countByKind scene
(2,2,1)
ghci> maxByArea scene
Just (Circle 3.0)
ghci> scaleAll 0.5 scene
[Circle 1.5,Rectangle 1.0 2.5,Square 2.0,Circle 0.5,Rectangle 1.5 1.5]
ghci> filterByMinArea 10 scene
[Circle 3.0,Rectangle 2.0 5.0,Square 4.0]
ghci> let sl = toShapeList scene
ghci> sl
SCons (Circle 3.0) (SCons (Rectangle 2.0 5.0) (SCons (Square 4.0) (SCons (Circle 1.0) (SCons (Rectangle 3.0 3.0) SEmpty))))
ghci> lengthSL sl
5
ghci> sumAreasSL sl
66.41592653589794
ghci> sumAreasSL sl
66.41592653589794
ghci> fromShapeList (mapSL (scale 2) sl)
[Circle 6.0,Rectangle 4.0 10.0,Square 8.0,Circle 2.0,Rectangle 6.0 6.0]
ghci> 
=========================================================== -}
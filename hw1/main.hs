-- Part A — Shape ADT and utilities

data Shape
  = Circle Float
  | Rectangle Float Float
  | Square Float
  deriving (Eq, Show)

-- area
area :: Shape -> Float
area (Circle r)      = pi * r * r
area (Rectangle w h) = w * h
area (Square s)      = s * s

-- perimeter
perimeter :: Shape -> Float
perimeter (Circle r)      = 2 * pi * r
perimeter (Rectangle w h) = 2 * (w + h)
perimeter (Square s)      = 4 * s       

-- scale
scale :: Float -> Shape -> Shape
scale k shape
  | k <= 0    = shape
scale k (Circle r)      = Circle (k * r)
scale k (Rectangle w h) = Rectangle (k * w) (k * h)
scale k (Square s)      = Square (k * s)

-- kind
kind :: Shape -> String
kind (Circle _)      = "Circle"
kind (Rectangle _ _) = "Rectangle"
kind (Square _)      = "Square"

------------------------------------------------------------
-- Part B — Lists of shapes

-- total area
totalArea :: [Shape] -> Float
totalArea = foldr (\sh acc -> area sh + acc) 0

-- total perimeter
totalPerimeter :: [Shape] -> Float
totalPerimeter = foldr (\sh acc -> perimeter sh + acc) 0

-- countByKind: (#circles, #rectangles, #squares)
countByKind :: [Shape] -> (Int, Int, Int)
countByKind = foldr step (0,0,0)
  where
    step (Circle _)      (c,r,s) = (c+1, r, s)
    step (Rectangle _ _) (c,r,s) = (c, r+1, s)
    step (Square _)      (c,r,s) = (c, r, s+1)

-- maxByArea
maxByArea :: [Shape] -> Maybe Shape
maxByArea []     = Nothing
maxByArea (x:xs) = Just (foldr bigger x xs)
  where
    bigger sh best
      | area sh > area best = sh
      | otherwise           = best

-- scaleAll
scaleAll :: Float -> [Shape] -> [Shape]
scaleAll k = map (scale k)

-- filterByMinArea
filterByMinArea :: Float -> [Shape] -> [Shape]
filterByMinArea t = filter (\sh -> area sh >= t)

------------------------------------------------------------
-- Part C — Recursive ADT practice (ShapeList)

data ShapeList
  = Empty
  | Cons Shape ShapeList
  deriving (Eq, Show)

-- convert [Shape] -> ShapeList
toShapeList :: [Shape] -> ShapeList
toShapeList []     = Empty
toShapeList (x:xs) = Cons x (toShapeList xs)

-- convert ShapeList -> [Shape]
fromShapeList :: ShapeList -> [Shape]
fromShapeList Empty        = []
fromShapeList (Cons x xs)  = x : fromShapeList xs

-- sum of areas
sumAreasSL :: ShapeList -> Float
sumAreasSL Empty        = 0
sumAreasSL (Cons x xs)  = area x + sumAreasSL xs

-- length of ShapeList
lengthSL :: ShapeList -> Int
lengthSL Empty        = 0
lengthSL (Cons _ xs)  = 1 + lengthSL xs

-- map over ShapeList
mapSL :: (Shape -> Shape) -> ShapeList -> ShapeList
mapSL _ Empty        = Empty
mapSL f (Cons x xs)  = Cons (f x) (mapSL f xs)

------------------------------------------------------------
-- Optional: local main for testing
main :: IO ()
main = do
  let shapes = [Circle 3, Rectangle 2 5, Square 4]
  print (totalArea shapes)
  print (totalPerimeter shapes)
  print (countByKind shapes)
  print (maxByArea shapes)
  print (scaleAll 2 shapes)
  print (filterByMinArea 10 shapes)
  let sl = toShapeList shapes
  print sl
  print (fromShapeList sl)
  print (sumAreasSL sl)
  print (lengthSL sl)
  print (mapSL (scale 2) sl)

module Closest

record Point where
  constructor MkPoint
  x : Double
  y : Double
  
instance Eq Point where
  (==) (MkPoint x1 y1) (MkPoint x2 y2) = x1 == x2 && y1 == y2

instance [xord] Ord Point where
  compare (MkPoint x1 y1) (MkPoint x2 y2) = compare x1 x2
  
instance [yord] Ord Point where
  compare (MkPoint x1 y1) (MkPoint x2 y2) = compare y1 y2    

basePoint : Point
basePoint = MkPoint 0 0

point11 : Point
point11 = MkPoint 1 1

point14 : Point
point14 = MkPoint 1 4

pts : List Point
pts = [basePoint,point11,point14]

sortedPts : List Point
sortedPts = sort @{yord} pts

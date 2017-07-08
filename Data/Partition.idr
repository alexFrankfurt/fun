module Data.Partition

import Data.SortedSet


%access export


Eq a => Eq (SortedSet a) where
  x == y = (with List toList x) == (with List toList y)

-- TODO: Efficient implementation
disjointProp : Ord a => (List $ SortedSet a) -> Bool
disjointProp [] = True
disjointProp (x :: xs)
  = (intersection x $ foldr union empty xs) == empty && disjointProp xs


a : Ord a => a -> a
a x = x

public export
data Partition : (a : Type) -> Type where
  MkPartition : Ord a =>
                (given : SortedSet a)
             -> (part : List $ SortedSet a)
             -> (prop : given = foldl union empty part)
             -> (disjointProp part = True)
             -> Partition a

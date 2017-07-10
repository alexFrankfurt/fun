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
data Partition : (a : Type)
              -> (prop : (List $ SortedSet a) -> Type)
              -> Type where
  MkPartition : Ord a =>
                (given : SortedSet a)
             -> (part : List $ SortedSet a)
             -> (partprf : given = foldl union empty part)
             -> prop part
             -> (disjointProp part = True)
             -> Partition a prop

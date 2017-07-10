module Algorithm.BPPOD.Model

import Algorithm.BPPOD.Property
import Data.Partition

import Data.SortedSet

%access public export

record Object where
  constructor MkObject
  weight : Double

Eq Object where
  a == b = (weight a) == (weight b)

Ord Object where
  compare (MkObject a) (MkObject b) = compare a b

implicit
conv : Double -> Object
conv = MkObject

interface BPPOD where
  -- Finite set of elements
  -- data A

  -- Lenghts of elements of A,
  -- should be inside (0, 1) interval.
  l : Double -> SBoundedDouble 0 1

  -- Partition of A into minimal by number set of parts Aᵢ
  -- such that Σ l(aᵢ) ≤ 1
  partition : Partition Object fun

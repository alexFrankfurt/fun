module Algorithm.BPPOD.Main

import Data.So
import Data.SortedSet

import Algorithm.BPPOD.Model
import Algorithm.BPPOD.Property
import Data.Partition

testData : SortedSet Object
testData = fromList [0.1, 0.2, 0.3, 0.4, 0.5,
                     0.6, 0.13, 0.27, 0.44, 0.91,
                     0.22, 0.14, 0.99, 0.11, 0.72]

construct : SortedSet Object -> List $ SortedSet Object

BPPOD where
  l x = case choose (x > 0 && x < 1) of
             Left p => [x;p]
  partition
    = MkPartition testData (construct testData)
                  ?refl ?rhs3 ?rhs4

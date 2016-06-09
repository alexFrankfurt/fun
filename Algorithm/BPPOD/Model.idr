module Algorithm.BPPOD.Model

import Algorithm.BPPOD.Property
import Data.Set

interface BPPOD
  -- Finite set
  data A

  -- Lenghts of elements of A,
  -- should be inside (0, 1) interval.
  l : A -> SBoundedDouble 0 1

  -- Partition of A into minimal by number set of parts Aᵢ
  -- such that Σ l(aᵢ) ≤ 1
  partition : A -> List $ Set A

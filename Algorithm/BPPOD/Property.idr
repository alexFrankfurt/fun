module Algorithm.BPPOD.Property

import Data.So
import Data.Set

-- TODO: Write generic bounded.
export
data SBoundedDouble : (a : Double) -> (b : Double) -> Type where
  MkSBD : (d : Double ** So (d > a && d < b)) -> SBoundedDouble a b

syntax "[" [a] ";" [p] "]" = MkSBD (a ** p)

export
BoundedToOne : Type
BoundedToOne = (d : Double ** So (d > 0 && d < 1))

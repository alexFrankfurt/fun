module Data.Partition

import Data.Set

-- I guess it requires bijection between types
data Partition : (a : Type) -> Type where
  MkPartition : Set $ Set a -> Partition a

-- bijection
infixl 4 <->
-- injection
infixl 4 ->-
-- surjection
infixl 4 -<-

data (<->) : Type -> Type -> Type where
  Mk : a -> b -> (<->) a b

data A = A1 | A2 | A3

fffn : (Int <-> Int) <-> Double



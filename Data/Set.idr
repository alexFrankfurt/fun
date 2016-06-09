module Data.Set

import Data.So

%access export

mutual
  public export
  data Set : (a : Type) -> Type where
    Single : a -> Set a
    OneMoreE : Eq a => (e : a) -> (s : Set a ** So $ not $ c e s) -> Set a

  c : Eq a => a -> Set a -> Bool
  c e (Single a) = e == a
  c e (OneMoreE a (s ** prf)) = e == a || c e s

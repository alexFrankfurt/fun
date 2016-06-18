module Data.Set

import Data.So

%default total
%access export

mutual
  public export
  data Set : (a : Type) -> Type where
    Empty : Set a
    Cons : Eq a => (e : a) -> (s : Set a ** So $ not $ c e s) -> Set a

  c : Eq a => a -> Set a -> Bool
  c e Empty = False
  c e (Cons a (s ** prf)) = assert_total $ e == a || c e s

data Elem : Eq a => a -> Set a -> Type where
  IsElem : Eq a => {e : a} -> Elem e (Cons e c)

elem : Eq a => a -> Set a -> Bool
elem e Empty = False
elem e (Cons a (s ** prf)) = e == a || elem e (assert_smaller (Cons a (s ** prf)) s)

dU : (b : Set a) -> (c : Set a)-> Set a
dU Empty Empty = Empty
dU Empty c     = c
dU c Empty     = c
dU (Cons e (s1 ** prfl)) s2
  = assert_total $ case choose (c e $ dU s1 s2) of
      Left prf => dU s1 s2
      Right prf => Cons e ((dU s1 s2) ** prf)

dU_preserves_inhabitants : Eq t => {e : t} -> Elem e s1 -> (s2 : Set t) -> Elem e $ dU s1 s2

Foldable Set where
  foldr f a Empty = a
  foldr f a (Cons e (s ** prf))
    = f e (foldr f a (assert_smaller (Cons e (s ** prf)) s))

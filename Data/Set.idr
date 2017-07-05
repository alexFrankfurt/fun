module Data.Set

import Data.So
import Dummy

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

c_empty : Eq a => (v : a) -> Data.Set.c v Empty = False
c_empty v = Refl

using (Eq a)
  data Elem : a -> Set a -> Type where
    IsElem : {e : a} -> Elem e (Cons e c)
    HasElem : {ne : a} -> Elem e s
                       -> (prf : So $ not $ c ne s)
                       -> Elem ne (Cons ne (s ** prf))

-- TODO: Uninhabited empty set

using (v:a)
  Eq a => Uninhabited (Elem v Empty) where
    uninhabited IsElem impossible

elem : Eq a => a -> Set a -> Bool
elem e Empty = False
elem e (Cons a (s ** prf)) = e == a || elem e (assert_smaller (Cons a (s ** prf)) s)

mutual
  partial
  U : (b : Set a) -> (c : Set a)-> Set a
  U Empty c     = c
  U c Empty     = c
  U (Cons e (s1 ** prfl)) s2
    = case choose (c e s2) of
        Left prf => Cons e (U s1 s2 ** ?prf)--lemma_U s2 s1 prf)
        Right prf => U (assert_smaller (Cons e (s1 ** prfl)) s1) s2

  lemma_U : Eq a => (s1 : Set a) -> (s2 : Set a) -> So (c e s1) -> So (c e $ U s1 s2)

    -- assert_total $ case choose (c e $ U s1 s2) of
    --   Left prf => U s1 s2
    --   Right prf => Cons e ((U s1 s2) ** prf)

-- U_preserves_inhabitants : Eq t => {e : t} -> Elem e s1 -> (s2 : Set t) -> Elem e $ U s1 s2

Foldable Set where
  foldr f a Empty = a
  foldr f a (Cons e (s ** prf))
      -- assert_smaller for s doesn't work
    = assert_total $ f e (foldr f a s)

vl : Set A
vl = Cons A1 (Empty ** Oh)

vll : Set A
vll = Cons A2 (vl ** Oh)

vlll : Set A
vlll = Cons A3 (vll ** Oh)

set : Set A
set = vlll

-- interface Constructors a where
--   css : List a

--   private prop : a <-> css

-- data Css : a -> Type where
--   NoCss : Css a
--   MoreCss : a -> Css a -> Css a

-- typed : Set A -> Type
-- typed Empty = A
-- typed (Cons v (s ** prf)) = ?rhs

-- lem : (css : Set A) -> typed css = A
-- lem Empty = Refl

-- lemma : typed Set.set = A
-- lemma = ?rhs
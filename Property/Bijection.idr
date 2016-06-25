module Property.Bijection

import Dummy

data Inj : a -> b -> Type where
  MkInj : (f : a -> b) ->
          (x : a) -> (y : a) ->
          (prf : f x = f y -> x = y) ->
          Inj a b

data Sur : a -> b -> Type where
  MkSur : (f : a -> b) ->
          (i : b) ->
          (x ** f x = i) ->
          Sur a b

data Bij : a -> b -> Type where
  MkBij : (i : Inj a b) -> (s : Sur a b) -> Bij a b

test : List a -> a -> Inj (List a) a

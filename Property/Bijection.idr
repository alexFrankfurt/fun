module Property.Bijection

import Dummy

%default total

-- fer : (f : a -> b)

data Inj : (a -> b) -> Type where
  MkInj : (f : a -> b) ->
          (req : (x, y : a) ->
                 (f x = f y -> x = y)) ->
          Inj f

data Sur : (a -> b) -> Type where
  MkSur : (f : a -> b) ->
          (req : (i : b) ->
                 (x ** f x = i)) ->
          Sur f

data Bij : (a -> b) -> Type where
  MkBij : (i : Inj f) -> (s : Sur f) -> Bij f

test : (f : List a -> a) -> Inj f

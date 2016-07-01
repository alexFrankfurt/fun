module Dummy

%access public export
--%default total

data A = A1 | A2 | A3

Eq A where
  A1 == A1 = True
  A2 == A2 = True
  A3 == A3 = True
  _  == _  = False

f : A -> A
f A1 = A1


data D : Type where
  Mk : (f : a -> a) ->
       (r : (x : a) ->
            (a -> a)) ->
       D

v : D
v = Mk f ?rhs

module Dummy

%access public export

data A = A1 | A2 | A3

Eq A where
  A1 == A1 = True
  A2 == A2 = True
  A3 == A3 = True
  _  == _  = False

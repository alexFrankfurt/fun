module TM.Utils

import TM.TM

%access export

hasOnlyBy : (a -> a -> Bool) -> List a -> List a -> Bool
hasOnlyBy p elems []        = True
hasOnlyBy p elems (x :: xs) =
  if elemBy p x elems then
    hasOnlyBy p elems xs
  else
    False

hasOnly : Eq a => List a -> List a -> Bool
hasOnly = hasOnlyBy (==)

namespace char
  toBinary : Char -> Nat
  toBinary '1' = 1
  toBinary '0' = 0

namespace string
  toBinary : String -> List Nat
  toBinary = (<$>) toBinary . unpack

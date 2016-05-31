module TM.Utils

import TM.TM
import TM.Types

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

toAlphabet : Char -> Alphabet
toAlphabet '0' = O
toAlphabet '1' = I

toAlphabetList : String -> List Alphabet
toAlphabetList = (<$>) toAlphabet . unpack

process' : TuringMachine => List Alphabet -> (List Alphabet, Integer)

changeState : TuringMachine => Q -> List Γ -> (Q, List Γ)
changeState q (x :: xs) = let (q', x', move) = δ (q, x)
  in ?rhs

[plain] Show a => Show (List a) where
  show [] = ""
  show (x::xs) = show x ++ show xs

showres : (List Alphabet, Integer) -> (String, Integer)
showres (l, r) = (show @{plain} l, r)

process : TuringMachine => String -> (String, Integer)
process = showres . process' . toAlphabetList

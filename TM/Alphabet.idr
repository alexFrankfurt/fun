module TM.Alphabet

%access public export

data Alphabet = O | I | S | E

Show Alphabet where
  show O = "O"
  show I = "I"
  show S = "#"
  show E = "␣"

toAlphabet : Char -> Alphabet
toAlphabet '0' = O
toAlphabet '1' = I
toAlphabet 'I' = I
toAlphabet 'O' = O
toAlphabet '#' = S

toAlphabetList : String -> List Alphabet
toAlphabetList = (<$>) toAlphabet . unpack

[plain] Show a => Show (List a) where
  show [] = ""
  show (x::xs) = show x ++ show xs

showres : (List Alphabet, Integer) -> (String, Integer)
showres (l, r) = (show @{plain} l, r)

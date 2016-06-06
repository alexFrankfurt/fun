module Tests

import Data.Vect

-- init : (s : String) -> {auto ok : isCons (unpack s) = True } -> String
-- init s = pack $ init $ unpack s

safeTail : Vect n a -> Maybe (Vect (n `minus` 1) a)
safeTail [] = Nothing
safeTail {n = S n} (x :: xs) = Just (rewrite minusZeroRight n in xs)

safeTail' : Vect n a -> {auto ok : LTE 1 n} -> Vect (n `minus` 1) a

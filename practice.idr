module Practice

import Data.Vect

res0 : Num a => (n : Nat ** Vect n a)
res0 = (_ ** [2,3])

res1 : (Ord a, Num a, Neg a) => List a
res1 = (map (*2)) flip (filter (>0)) [1,2,-1,3]

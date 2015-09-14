module Mergesort

toNatDouble : Double -> Nat
toNatDouble d = fromIntegerNat (cast d)

combine : Ord a => List a -> List a -> List a
combine []       ys     = ys
combine xs       []     = xs
combine (x::[]) (y::[]) = if x > y
                             then y :: x :: []
                             else x :: y :: []
combine (x::xs) (y::ys) = if x < y
                             then x :: combine xs (y::ys)
                             else y :: combine (x::xs) ys

mergeSort : Ord a => List a -> List a
mergeSort []             = []
mergeSort (x::[])        = x::[]
mergeSort l @ (x::y::xs) = 
  let n = fromInteger (toIntegerNat (with List length l)) / 2 in
      combine (mergeSort (with List take (toNatDouble n) l))
              (mergeSort (with List drop (toNatDouble n) l))

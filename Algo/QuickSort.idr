module Algo.QuickSort

import Data.Vect

sorter : Ord a => List a -> a -> List a
sorter []         el
  = el :: []
sorter (x::[])    el
  = if x > el
       then el :: x :: []
       else x :: el :: []
sorter (x::y::xs) el
  = let l      = x :: y :: xs
        laste  = last l
        firste = head l
    in case (head l > el, last l < el) of
            (True, True) => laste :: sorter (init (y::xs)) el
            (True, False) => sorter (init (x :: y :: xs)) el ++ laste :: []
            (False, True) => firste :: sorter (y :: xs) el
            (False, False) => firste :: sorter (init (y :: xs)) el ++ laste :: []

sep : Ord a => List a -> Maybe Nat -> (List a, List a)
sep lis bo
  = case bo of
         Just n => let beg = take n lis
                       end = drop (n + 1) lis
                   in (beg, end)
         Nothing => (lis, [])

quickSortImpl : Ord a => List a -> a -> List a
quickSortImpl [] el
  = el :: []
quickSortImpl (x::[]) el
  = if x > el
       then el :: x :: []
       else x :: el :: []
quickSortImpl l @ (x::y::xs) el
  = let l'  = sorter l el
        ind = elemIndex el l'
    in case ind of
            Just n => let fs = with List take n l'
                          ss = with List drop (n + 1) l'
                      in case (fs, ss) of
                              (f::fs, s::ss) => quickSortImpl (init $ f::fs) (last (f::fs)) ++ (el :: []) ++ quickSortImpl (init $ s::ss) (last $ s::ss)
                              ([], s::ss) => el :: quickSortImpl (init $ s::ss) (last $ s::ss)
                              (f::fs, []) => quickSortImpl (init $ f::fs) (last $ f::fs) ++ el :: []
                              _ => l'
            Nothing => l'

quickSort : Ord a => List a -> List a
quickSort l @ (x :: xs) = quickSortImpl (init l) (last l)

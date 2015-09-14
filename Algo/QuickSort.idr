module QuickSort

import Data.Vect

test : Ord a => a -> a -> String
test a b = if a <= b
              then "Less"
              else "More"
              
sorter : Ord a => List a -> a -> List a
sorter []         el = el :: []
sorter (x::[])    el = if x > el
                          then el :: x :: []
                          else x :: el :: []
sorter (x::y::xs) el = let l      = x :: y :: xs 
                           laste  = last l 
                           firste = head l in
                             case (head l > el, last l < el) of
                               (True, True) => laste :: sorter (init (with List y::xs)) el
                               (True, False) => sorter (init (with List x :: y :: xs)) el ++ laste :: []
                               (False, True) => firste :: sorter (with List y :: xs) el
                               (False, False) => firste :: sorter (init (with List y :: xs)) el ++ laste :: []


sep : Ord a => List a -> Maybe Nat -> (List a, List a)
sep lis bo = case bo of
                  Just n => let beg = take n lis 
                                end = drop (n + 1) lis in
                                (beg, end)
                  Nothing => (lis, [])

quickSortImpl : Ord a => List a -> a -> List a
quickSortImpl [] el      = el :: []
quickSortImpl (x::[]) el = if x > el
                              then el :: x :: []
                              else x :: el :: []                              
quickSortImpl l @ (x::y::xs) el = let l'  = sorter l el
                                      ind = elemIndex el l' in
                                      case ind of
                                           Just n => let fs = with List take n l'
                                                         ss = with List drop (n + 1) l' in
                                                         case (fs, ss) of
                                                           (f::fs, s::ss) => quickSortImpl (init (with List f::fs)) (last (with List f::fs)) ++ (el :: []) ++ quickSortImpl (init (with List s::ss)) (last (with List s::ss))
                                                           ([], s::ss) => el :: quickSortImpl (init (with List s::ss)) (last (with List s::ss))
                                                           (f::fs, []) => quickSortImpl (init (with List f::fs)) (last (with List f::fs)) ++ el :: []
                                                           _ => l'
                                           Nothing => l' 

quickSort : Ord a => List a -> List a
quickSort l @ (x :: xs) = quickSortImpl (init l) (last l)

main : IO ()
main = putStrLn $ test 8 4

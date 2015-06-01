module Exer

import Data.Vect

repeat : (n : Nat) -> a -> Vect n a
repeat Z     _ = []
repeat (S k) a = a :: repeat k a

vtake : (n : Nat) -> Vect (n + t) a -> Vect n a
vtake Z     _       = []  
vtake (S t) (x::xs) = x::vtake t xs 

vdrop : (n : Nat) -> Vect (n + t) a -> Vect t a
vdrop Z     xs      = xs
vdrop (S k) (x::xs) = vdrop k xs

Matrix : Type -> Nat -> Nat -> Type
Matrix a n m = Vect n $ Vect m a

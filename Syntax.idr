module Syntax

swap' : (a, b) -> (b, a)
swap' (x, y) = (y, x)

syntax swap [fst] "with" [snd] = swap' (fst, snd)

syntax [arg1]"."[f] = f arg1

res0 : (Integer, Integer)
res0 = (10, 8)

-- res0_eq : res0 = (10, 8)
-- res0_eq = Refl

f : Nat -> Nat -> Nat
f x y = x * 2 * y

res1 : Nat
res1 = 2.f 4

infixr 10 :.:

(:.:) : Nat -> Nat -> Nat
(:.:) k l = min k l

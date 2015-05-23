module Synt

swap' : (a, b) -> (b, a)
swap' (x, y) = (y, x)

syntax swap [fst] "with" [snd] = swap' (fst, snd)

res0 : (Integer, Integer)
res0 = swap 8 with 10

res0_eq : res0 = (10, 8)
res0_eq = Refl

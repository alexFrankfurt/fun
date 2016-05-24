module Model

interface Top a where
  showTy : a -> String

data MidL = MkMidL Double

data MidR = MkMidR Double

Top MidL where
  showTy _ = "MidL"

Top MidR where
  showTy _ = "MidR"

td1 : MidL
td1 = MkMidL 10

td2 : MidR
td2 = MkMidR 20

data Relation = Rel1
              | Rel2
              | Rel3
              | Rel4
              | Rel5

interface Relationable a where
  relate : (Relationable b) => a -> b -> Relation

data Point = MkPoint Double Double

data Line = MkLine Point Point

Relationable Point where
  relate (MkPoint a b) (MkPoint c d) = Rel1
  relate (MkPoint a b) (MkLine c d) = Rel2

Relationable Line where
  relate (MkLine a b) (MkLine c d) = Rel4
  relate (MkLine a b) (MkPoint c d) = Rel3

tdp1 : Point
tdp1 = MkPoint 1 (-1)

tdp2 : Point
tdp2 = MkPoint (-1) (-1)

tdl1 : Line
tdl1 = MkLine tdp1 tdp2

tdl2 : Line
tdl2 = MkLine (MkPoint (-111) 88) (MkPoint 0 0)

res0 : Relation
res0 = tdp1 `relate` tdp2

res1 : Relation
res1 = tdp1 `relate` tdl1

res2 : Relation
res2 = tdl1 `relate` tdl2

res3 : Relation
res3 = tdl2 `relate` tdp2

res : (Relation, Relation, Relation, Relation)
res = (res0, res1, res2, res3)

-- for a while it's good solution for
-- relation problem
interface Relater a b where
  relater : a -> b -> Relation

Relater Point Line where
  relater (MkPoint x y) (MkLine p1 p2) = Rel1

Relater Point Point where
  relater (MkPoint x y) (MkPoint z w) = Rel2

res4 : Relation
res4 = relater tdp1 tdp2

res5 : Relation
res5 = relater tdp1 tdl2

--- Check inheritance overriding behaviour
interface FiniteAutomata a where
  f : a -> (Double, Double)

namespace TM
  interface FiniteAutomata a => TM a where
    f : a -> Nat

FiniteAutomata Integer where
  f x = (1.1, 1.1)

TM Integer where
  f x = 2

func : TM a => a -> Nat
func x = f x

-- Data inside interfaces
interface C where
  data S : Type

data Foo = Bar

C where
  S = Foo

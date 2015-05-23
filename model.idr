module Model

class Top a where
  showTy : a -> String

data MidL = MkMidL Double

data MidR = MkMidR Double

instance Top MidL where
  showTy _ = "MidL"

instance Top MidR where
  showTy _ = "MIdR"

td1 : MidL
td1 = MkMidL 10

td2 : MidR
td2 = MkMidR 20

-- fun : Top a => a -> String
-- fun b = case b of
--               => "Llllll"
--              (MkMidR c) => "RRRRRRRR"

-- res0 : String
-- res0 = fun td1

-- res1 : String
-- res1 = fun td2

data Relation = Rel1
              | Rel2
              | Rel3
              | Rel4
              | Rel5

class Relationable a where
  relate : (Relationable b) => a -> b -> Relation
  
data Point = MkPoint Double Double

data Line = MkLine Point Point

instance Relationable Point where
  relate (MkPoint a b) (MkPoint c d) = Rel1
  relate (MkPoint a b) (MkLine c d) = Rel2

instance Relationable Line where
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

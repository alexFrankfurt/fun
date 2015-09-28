module BinaryTree

data BinaryTree : (keyT : Type) -> (dataT : Type) -> Type where
     EmptyTree  : BinaryTree keyT dataT
     Node       : keyT -> dataT -> (BinaryTree keyT dataT)
                  -> (BinaryTree keyT dataT) -> BinaryTree keyT dataT
                  
Leaf : a -> b -> BinaryTree a b
Leaf keyv val = Node keyv val EmptyTree EmptyTree

key : BinaryTree a b -> Maybe a
key EmptyTree = Nothing
key (Node x y z w) = Just x

notEmptyTree : BinaryTree a b -> Bool
notEmptyTree EmptyTree = False
notEmptyTree (Node x y z w) = True

leftMost : (t : BinaryTree a b) 
           -> {auto ok : notEmptyTree t = True} 
           -> (BinaryTree a b -> BinaryTree a b -> BinaryTree a b)
leftMost (Node x y z w) = case z of
                               EmptyTree => Node x y
                               t @ (Node x y z w) => leftMost t
                               
withoutLeftMost : (t : BinaryTree a b)
                  -> {auto ok : notEmptyTree t = True} 
                  -> BinaryTree a b
withoutLeftMost (Node x y z w) = case z of
                                      EmptyTree => w
                                      t @ (Node x y z w) => Node x y (withoutLeftMost t) w

value : BinaryTree Integer Double
value = Node 1 1 EmptyTree EmptyTree

dataValue : BinaryTree Integer Double
dataValue = 
  Node 8 9.0 (Node 1 1 EmptyTree EmptyTree) 
             (Node 2 2.0 EmptyTree EmptyTree)

find : Ord a => a -> BinaryTree a b -> Maybe b
find x EmptyTree = Nothing
find x (Node y dat w s) = if x <= y
                             then if x == y
                                     then Just dat
                                     else find x w
                             else find x s

insert : Ord a => (a, b) -> BinaryTree a b -> BinaryTree a b
insert (k, b)       EmptyTree      = Leaf k b
insert dat @ (k, b) (Node x y z w) = if k < x
                                        then Node x y (insert dat z) w
                                        else Node x y z $ insert dat w

delete : Ord a => a -> BinaryTree a b -> BinaryTree a b
delete x EmptyTree = EmptyTree
delete x (Node y z w s) =
  if x <= y
    then if x == y
            then case (w, s) of
                      (EmptyTree, EmptyTree) => EmptyTree
                      (EmptyTree, Node x y z w) => Node x y z w
                      (Node x y z w, EmptyTree) => Node x y z w
                      (Node x y z w, Node s m n k) =>
                        leftMost (Node s m n k) (Node x y z w) (withoutLeftMost $ Node s m n k)
            else Node y z (delete x w) s
    else Node y z w $ delete x s

rotateLeft : Ord a => a -> BinaryTree a b -> BinaryTree a b
rotateLeft x EmptyTree = EmptyTree
rotateLeft x (Node y z w s) with (x == y)
  | True  = Node y z w s
  | False 
    = if x < y
         then Node y z (rotateLeft x w) s
         else case key s of
                   -- Make a proof: key s = Nothing <=> s = EmptyTree
                   Nothing => Node y z w EmptyTree
                   Just a => if x == a
                                then case s of
                                     EmptyTree => Node y z w s
                                     (Node ry rz rw rs) => Node ry rz (Node y z w rw) rs
                                else Node y z w $ rotateLeft x $ Node y z w s

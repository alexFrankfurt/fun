module Structure.BinaryTree

import Data.Vect
import Effects
import Effect.StdIO
import Effect.State

data BinaryTree : (keyT : Type) -> (dataT : Type) -> Type where
     EmptyTree  : BinaryTree keyT dataT
     Node       : keyT -> dataT -> (BinaryTree keyT dataT)
                  -> (BinaryTree keyT dataT) -> BinaryTree keyT dataT
                  
Leaf : a -> b -> BinaryTree a b
Leaf keyv datav = Node keyv datav EmptyTree EmptyTree

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
                               t @ (Node xl yl zl wl) => leftMost t
                               
withoutLeftMost : (t : BinaryTree a b)
                  -> BinaryTree a b
withoutLeftMost EmptyTree = EmptyTree                  
withoutLeftMost (Node x y z w) = case z of
                                      EmptyTree => EmptyTree
                                      t @ (Node xl yl zl wl) => Node x y (withoutLeftMost zl) w

value : BinaryTree Integer Double
value = Node 1 1 EmptyTree EmptyTree

t0 : BinaryTree Integer Double
t0 = Node 68 3 EmptyTree EmptyTree

t1 : BinaryTree Integer Double
t1 = Node 54 3 EmptyTree EmptyTree

t2 : BinaryTree Integer Double
t2 = Node 77 3 t0 EmptyTree

t3 : BinaryTree Integer Double
t3 = Node 60 3 t1 t2

dataValue : BinaryTree Integer Double
dataValue = Node 50 9.0 (Node 28 1 EmptyTree EmptyTree)
                        t3

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
                      (EmptyTree, Node yr zr wr sr) => Node yr zr wr sr
                      (Node yl zl wl sl, EmptyTree) => Node yl zl wl sl
                      (Node yl zl wl sl, Node yr zr wr sr) =>
                        leftMost (Node yr zr wr sr) (Node yl zl wl sl) (withoutLeftMost $ Node yr zr wr sr)
            else Node y z (delete x w) s
    else Node y z w $ delete x s

rotateLeft : Ord a => a -> BinaryTree a b -> BinaryTree a b
rotateLeft x EmptyTree = EmptyTree
rotateLeft x (Node y z w s) with (x == y)
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
                                else Node y z w $ rotateLeft x $ s
  | True = Node y z w s
                                

elemWidth : Nat
elemWidth = 2

width : BinaryTree a b -> Nat
width EmptyTree = Z
width (Node x y z w) = width z + elemWidth + width w

times : String -> Nat -> String
times s Z = ""
times s (S k) = s ++ times s k

-- pprint : Show a => BinaryTree a b -> String
-- pprint EmptyTree = ""
-- pprint (Node x y z w) 
--   = --with String (++)
--       with String (++)
--          (with String (++) 
--                 (with String (++) (" " `times` width z) $ show x)
--                 (" " `times` width w))
--          "\n"

bracedString : Show a => BinaryTree a b -> String
bracedString EmptyTree = ""
bracedString (Node x y z w) 
  = with Strings (++)
     (with Strings (++)
      (show x)
      (with Strings (++)
         (with Strings (++)
              "{ "
              $ bracedString z)
         " }"))
      (with Strings (++)
        (with Strings (++)
          "{ "
          $ bracedString w)
        " }")
      
  

toString : Show a => BinaryTree a b -> String
toString EmptyTree = ""
toString (Node x y z w) = show x ++ show ' ' ++ bracedString z ++ bracedString w

testsearch : Integer -> BinaryTree Integer Double -> Double
testsearch v t = case find v t of
                      Nothing => -1
                      Just a => a
                      
testinsert : (Integer, Double) -> BinaryTree Integer Double -> BinaryTree Integer Double
testinsert = insert

testdelete : Integer -> BinaryTree Integer Double -> BinaryTree Integer Double
testdelete = delete

testrotateleft : Integer -> BinaryTree Integer Double -> BinaryTree Integer Double
testrotateleft = rotateLeft


onlyEmpty : Vect n $ BinaryTree a b -> Bool
onlyEmpty ts = foldl check True ts
  where 
    check : Bool -> BinaryTree a b -> Bool
    check False _              = False
    check True  EmptyTree      = True
    check True  (Node _ _ _ _) = False
    
total
plus_commutes_S : (k : Nat) -> (m : Nat) -> S (plus m k) = plus m (S k)
plus_commutes_S k Z = Refl
plus_commutes_S k (S j) = rewrite plus_commutes_S k j in Refl    
    

rowPrinter : Show a
          => Vect n $ BinaryTree a b 
          -> Vect (S n) $ BinaryTree a b 
          -> Eff (Vect (n + (S n)) $ BinaryTree a b, Vect (2 * (S n)) $ BinaryTree a b) [STDIO]
rowPrinter []      (lc::[]) 
  = case lc of
         EmptyTree => do pure ([EmptyTree], [EmptyTree, EmptyTree])
         Node k d l r => 
           do putStr $ " " `times` width l
              putStr $ show k
              putStrLn $ " " `times` width r
              pure ([lc], [l, r])
rowPrinter (p::ps) (c::cs) 
  = case p of
         EmptyTree => ?rhs
         Node _ _ _ _ =>
           case c of
             EmptyTree => ?rhs1
             Node k d l r =>
               do putStr $ " " `times` width l
                  putStr $ show k
                  putStr $ " " `times` width r
                  (nps, ncs) <- rowPrinter ps cs
                  pure (c::nps, l::r::ncs)
             


recPrinter : Show a => 
    Eff Bool [STDIO, 
             'Prevs ::: STATE (Vect n (BinaryTree a b)),
             'Curs ::: STATE (Vect (S n) (BinaryTree a b))]
             (\ok => if ok then [STDIO,
                                'Prevs ::: STATE (Vect (n + (S n)) (BinaryTree a b)),
                                'Curs ::: STATE (Vect (2 * (S n)) (BinaryTree a b))]
                           else [STDIO,
                                'Prevs ::: STATE (Vect n (BinaryTree a b)),
                                'Curs ::: STATE (Vect (S n) (BinaryTree a b))])
recPrinter = case not $ onlyEmpty !('Curs :- get) of
                  False => pureM False
                  True  => do (np, nc) <- (rowPrinter !('Prevs :- get) !('Curs :- get))
                              'Prevs :- putM np
                              'Curs :- putM nc
                              pureM True





treePrinter : Show a => (t : BinaryTree a b) -> IO ()
treePrinter r @ (Node k v l r) = do putStr $ show " " `times` width l
                                    putStr $ show k
                                    putStrLn $ show " " `times` width r
                                    runInit [(), 'Prevs := [r], 'Curs := [l, r]] recPrinter
                                    pure ()
                                
                                





rec : Nat -> IO ()
rec Z = pure ()
rec (S k) = do putStrLn $ show $ S k
               rec k

--pprint : BinaryTree a b -> IO ()

main : IO ()
main = rec 8 -- print $ show $ pprint dataValue

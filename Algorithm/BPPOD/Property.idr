module Algorithm.BPPOD.Property

import Data.So
import Data.Set

-- Write generic bounded
public export
data SBoundedDouble : (a : Double) -> (b : Double) -> Type where
  MkSBD : (d : Double ** So (d > a && d < b)) -> SBoundedDouble a b

singleS : Set Int
singleS = Single 1

biggerS : Set Int
biggerS = case choose (c 2 singleS) of
            Left p => singleS
            Right p => OneMoreE 2 (singleS ** p)

-- TODO: partition property
data Partition : (a : Type) -> Type where
  MkPartition : List (List a) -> Partition a

Bounded : Type
Bounded = (d : Double ** So (d > 0 && d < 1))

test : SBoundedDouble 1 2 -> Double
test (MkSBD (x ** pf)) = x

strToDouble : String -> Double
strToDouble = cast

main : IO ()
main = do putStr "Enter number: "
          let x = strToDouble !getLine
          case choose (x > 1 && x < 2) of
            Left p => do putStrLn $ cast $ test (MkSBD (x ** p))
            Right p => do putStrLn "Fail"

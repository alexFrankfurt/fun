module Algorithm.BPPOD.Property

import Data.So

-- Write generic bounded
public export
data SBoundedDouble : (a : Double) -> (b : Double) -> Type where
  MkSBD : (d : Double ** So (d > a && d < b)) -> SBoundedDouble a b

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

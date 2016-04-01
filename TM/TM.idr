module TM.TM

import TM.Types
import TM.Utils

data ComputationalModel : Type where
  TuringMachine :
    Q -> Σ -> Γ ->
    (δ : (Q, Γ) -> (Q, Γ, Move)) ->
    q₀ -> qₐ -> qᵣ -> ComputationalModel

testData : String
testData = "10101100101"

main : IO ()
main = do putStrLn "Enter bits"
          input <- getLine
          print $ toBinary input

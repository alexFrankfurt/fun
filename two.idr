module Main

foo : List Int -> List Int
foo xs = if List.length xs < 4
            then ?case_one
            else ?case_two

assert_1 : Bool
assert_1 = foo [1,2,3] == [3,2,1]

proof_of_assert_1 : foo [1,2,3] = List.reverse [1,2,3]

case_one_proof : (l : List Int)
              -> (length l < 4) = True
              -> foo l = reverse l
                  
main : IO ()
main = putStrLn $ show $ foo [1,2,3]

---------- Proofs ----------

Main.case_one = proof
  intro xs
  refine List.reverse
  exact xs


